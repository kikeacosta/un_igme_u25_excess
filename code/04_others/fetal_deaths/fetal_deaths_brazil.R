library(here)
source(here("Code", "00_functions.R"))
library(read.dbc)

# codes
# =====
reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

muncodes <- 
  read_csv("Data/brazil/br-city-codes.csv")


# births 
# ======
bts_10_20 <- 
  read_rds("Output/births/brazil_monthly_births_state.rds")

# ============

# Fetal deaths
# ============
# identify all files .dbc in directory
files <- list.files("Data/Brazil/fetal_deaths/", "dbc|DBC")
file_paths <- paste0("Data/Brazil/fetal_deaths/", files)
# read them all
fds <- list()
for(i in file_paths){
  fds[[i]] <- 
    read.dbc(i)
}

fds2 <- 
  fds %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(date = dmy(DTOBITO),
         year = year(date))

# gestation weeks per year, including NAs
gest_time_year <- 
  table(fds2$year, fds2$GESTACAO, useNA = c("ifany"))

write.excel(gest_time_year)



# monthly by state
# =================

muncodes2 <- 
  muncodes %>% 
  select(name, state_iso = state, muncode = idIBGE) %>% 
  mutate(muncode = str_sub(muncode, 1, 6) %>% as.double()) %>% 
  left_join(reg_codes)

fds_municip <- 
  fds2 %>% 
  mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
                           GESTACAO %in% 3:6 ~ "28+",
                           TRUE ~ NA_character_),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         muncode = as.double(as.character(CODMUNOCOR)),
         date = make_date(d = 15, m = month_d, y = year_d)) %>% 
  select(date, 
         muncode,
         weeks) %>% 
  group_by(date, muncode, weeks) %>% 
  summarise(fds = n()) %>% 
  ungroup() %>% 
  left_join(muncodes2)

ref_state <- 
  read_csv2("Data/Brazil/fetal_deaths/annual_fetal_deaths_by_state.csv",
           skip = 4) %>%
  drop_na(Total) %>% 
  select(-Total) %>% 
  rename(state_raw = 1) %>% 
  filter(state_raw != "Total") %>% 
  gather(-state_raw, key = year, value = fds_ref) %>% 
  mutate(state_num = str_sub(state_raw, 1, 2) %>% as.double(),
         year = year %>% as.double()) %>% 
  left_join(reg_codes) %>% 
  select(year, state_iso, fds_ref)

fds_state_compare <- 
  fds_municip %>% 
  mutate(year = year(date)) %>% 
  group_by(year, state_iso) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  left_join(ref_state) %>% 
  mutate(diff = fds - fds_ref)

# almost identical match!!!!!!!!!!!!!!!!!!

fds_state_mth <- 
  fds_municip %>% 
  group_by(date, state_iso, weeks) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  left_join(reg_codes) %>% 
  select(date, state_iso, state_name, region, weeks, fds)

assign_unk_weeks <- 
  function(chunk){
    tot <- 
      chunk %>% 
      summarise(tot = sum(fds)) %>% 
      pull(tot)
    
    chunk %>% 
      filter(!is.na(weeks)) %>% 
      mutate(fds = round(tot * fds / sum(fds), 0))
  }

fds_state_mth2 <- 
  fds_state_mth %>% 
  group_by(date, state_iso) %>% 
  do(assign_unk_weeks(chunk = .data)) %>% 
  drop_na(state_iso)

fds_state_mth2_all_wks <- 
  fds_state_mth2 %>% 
  group_by(date, state_iso, state_name, region) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  mutate(weeks = "total")

fds_state_mth3 <- 
  fds_state_mth2 %>% 
  bind_rows(fds_state_mth2_all_wks)


# adding birth counts by state
# 

fds_state_mth4 <- 
  fds_state_mth3 %>% 
  left_join(bts_10_20) %>% 
  filter(date >= "2015-01-01") %>% 
  arrange(date) %>% 
  group_by(state_iso, weeks) %>% 
  mutate(t = 1:n(),
         ws = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup()

fds_region_mth <- 
  fds_state_mth4 %>% 
  group_by(date, region, weeks) %>% 
  summarise(fds = sum(fds),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  group_by(region, weeks) %>% 
  mutate(t = 1:n(),
         ws = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup() 


# ==================

est_baseline <- function(db){
  
  base_gam <-
    gam(fds ~
          t +
          s(month, bs = 'cp') +
          offset(log(exposure)),
        weights = ws,
        data = db,
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
  db %>% 
    mutate(bsn = resp$fit,
           ul = bsn + 1.96 * resp$se.fit,
           ll = bsn - 1.96 * resp$se.fit,
           p_score = fds / bsn,
           fds_r = fds / exposure,
           bsn_r = bsn / exposure,
           ll_r = ll / exposure,
           ul_r = ul / exposure)
}


region_bsln <- 
  fds_region_mth %>% 
  arrange(region, date) %>% 
  mutate(exposure = fds + bts,
         month = month(date)) %>% 
  group_by(region, weeks) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))

state_bsln <- 
  fds_state_mth4 %>% 
  arrange(state_iso, date) %>% 
  mutate(exposure = fds + bts,
         month = month(date)) %>% 
  group_by(state_iso, weeks) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))

# =======

# saving estimates
write_rds(region_bsln, "Output/excess/brazil_monthly_excess_fetal_deaths_region.rds")
write_rds(state_bsln, "Output/excess/brazil_monthly_excess_fetal_deaths_state.rds")


# plots
# =====

# regions
# ~~~~~~~

# counts
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, fds))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_grid(weeks ~ region, scales = "free")+
  theme_bw()

# rates
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, fds_r))+
  geom_line(aes(date, bsn_r))+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  facet_grid(weeks ~ region, scales = "free")+
  theme_bw()

# p-scores
region_bsln %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, p_score), alpha = 0.5)+
  geom_point(aes(date, p_score_un), col = "red", alpha = 0.5)+
  facet_wrap(~ region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

# p-scores
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, p_score), alpha = 0.5)+
  geom_point(aes(date, p_score_un), col = "red", alpha = 0.5)+
  facet_grid(weeks ~ region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()


# states
# ~~~~~~

# counts
state_bsln %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, fds))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(~ state_iso, scales = "free")+
  theme_bw()

# rates
state_bsln %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, fds_r))+
  geom_line(aes(date, bsn_r))+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  facet_wrap(~ state_iso, scales = "free")+
  theme_bw()


# ==============





# previous
# =======


# fds3 <- 
#   fds2 %>% 
#   mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
#                            GESTACAO %in% 3:6 ~ "28+",
#                            TRUE ~ NA_character_),
#          region = case_when(NATURAL %in% 811:817 ~ "N",
#                             NATURAL %in% 821:829 ~ "NE",
#                             NATURAL %in% 831:835 ~ "SE",
#                             NATURAL %in% 841:843 ~ "S",
#                             NATURAL %in% 850:853 ~ "CW",
#                             TRUE ~ "other"), 
#          region_rep = case_when(UFINFORM %in% 811:817 ~ "N",
#                                 UFINFORM %in% 821:829 ~ "NE",
#                                 UFINFORM %in% 831:835 ~ "SE",
#                                 UFINFORM %in% 841:843 ~ "S",
#                                 UFINFORM %in% 850:853 ~ "CW",
#                                 TRUE ~ "other"),
#          date_d = dmy(DTOBITO),
#          year_d = year(date_d),
#          month_d = month(date_d),
#          date = make_date(d = 15, m = month_d, y = year_d)) %>% 
#   select(date, 
#          place = LOCOCOR, 
#          death_delivery = OBITOPARTO, 
#          weeks,
#          state = NATURAL,
#          region,
#          region_rep) %>% 
#   group_by(date, place, state, region, region_rep, weeks) %>% 
#   summarise(fds = n()) %>% 
#   ungroup()
# 
# place <- 
#   fds3 %>% 
#   mutate(place2 = case_when(place %in% 1:2 ~ "hosp",
#                             place %in% 3:5 ~ "other",
#                             TRUE ~ "nas")) %>% 
#   group_by(date, region, place2) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() %>% 
#   filter(place2 != "nas") %>% 
#   group_by(date, region) %>% 
#   mutate(prop = fds / sum(fds))
# 
# place %>% 
#   filter(place2 == "other",
#          date >= "2015-01-01") %>% 
#   ggplot()+
#   geom_line(aes(date, prop))+
#   facet_wrap(~region)
# 
# state <- 
#   fds3 %>% 
#   filter(date >= "2015-01-01") %>% 
#   group_by(date, state) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# state %>% 
#   ggplot()+
#   geom_line(aes(date, fds))+
#   facet_wrap(~state, scales = "free")
# 
# 
# region <- 
#   fds3 %>% 
#   filter(date >= "2015-01-01") %>%
#   group_by(date, region) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# region %>% 
#   ggplot()+
#   geom_line(aes(date, fds))+
#   facet_wrap(~region, scales = "free")
# 
# 
# region_rep <- 
#   fds3 %>% 
#   filter(date >= "2015-01-01") %>%
#   group_by(date, region_rep) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# region_rep %>% 
#   ggplot()+
#   geom_line(aes(date, fds))+
#   facet_wrap(~region_rep, scales = "free")
# 
# 
# # amazonas
# state %>% 
#   filter(state == 813) %>% 
#   ggplot()+
#   geom_line(aes(date, fds))
# 
# # sao paulo
# state %>% 
#   filter(state == 813) %>% 
#   ggplot()+
#   geom_line(aes(date, fds))
# 
# 
# all <- 
#   fds3 %>% 
#   filter(date >= "2015-01-01") %>% 
#   group_by(date) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# all %>% 
#   ggplot()+
#   geom_line(aes(date, fds))
# 
# 
# 
# # weeks
# 
# weeks <- 
#   fds3 %>% 
#   filter(date >= "2015-01-01") %>% 
#   group_by(date, weeks) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# weeks %>%
#   filter(weeks == "<27") %>% 
#   ggplot()+
#   geom_line(aes(date, fds))
# 
# 
# weeks %>%
#   filter(weeks == "28+") %>% 
#   ggplot()+
#   geom_line(aes(date, fds))
# 
# 
# # annual
# 
# annual_reg <- 
#   fds3 %>% 
#   mutate(year = year(date)) %>%  
#   group_by(year, weeks, region) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# annual_reg %>%
#   filter(weeks == "28+",
#          year >= 2015) %>% 
#   ggplot()+
#   geom_point(aes(year, fds))+
#   facet_wrap(~region, scales = "free")
# 
# 
# annual_state <- 
#   fds3 %>% 
#   mutate(year = year(date)) %>%  
#   group_by(year, weeks, state) %>% 
#   summarise(fds = sum(fds)) %>% 
#   ungroup() 
# 
# annual_state %>%
#   filter(weeks == "28+",
#          year >= 2016) %>% 
#   ggplot()+
#   geom_point(aes(year, fds))+
#   facet_wrap(~state, scales = "free")
# 
# 
# 
# 
# 
# 
