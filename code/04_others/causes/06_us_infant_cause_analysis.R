source("Code/causes/00_functions_cause_analyses.R")

# deaths from CDC Wonder ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_css <- 
  read_tsv("Data/USA/wonder_infants_causes.txt") %>% 
  select(-1,
         -3,
         -4,
         -10,
         year = Year,
         age = 5,
         cause = 6,
         code = 7,
         dts = 8,
         bts_cdc = 9) %>% 
  drop_na(year) %>% 
  mutate(age = factor(age, levels = c("1d", "1-6d", "7-27d", "28-364d")))
# unique(db_css$age)

db_tot <- 
  read_tsv("Data/USA/wonder_infant.txt") %>% 
  select(-1,
         -3,
         -4,
         -8,
         year = Year,
         age = 5,
         dts_tot = 6,
         bts_cdc = 7) %>% 
  drop_na(year) %>% 
  mutate(age = factor(age, levels = c("1d", "1-6d", "7-27d", "28-364d")))

# births from STFF database ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading data from Human Fertility Database (HFD)
db_stff <- read_csv("https://www.humanfertility.org/STFF/stff.csv")

bs_stf <- 
  db_stff %>% 
  select(CountryCode, year = Year, TOT) %>% 
  mutate(bts_stff = TOT %>% as.double()) %>% 
  filter(CountryCode == "USA",
         year %in% 2015:2020) %>% 
  drop_na() %>% 
  select(-TOT, -CountryCode) 


chapters <- 
  read_xlsx("Data/USA/infant_causes_summary.xlsx",
            sheet = "chapters")

code_chapters <- 
  chapters %>% 
  pull(code)

name_chapters <- 
  chapters %>% 
  select(code, name)

db_chp <- 
  db_css %>% 
  filter(code %in% code_chapters) %>% 
  left_join(db_tot) %>% 
  left_join(bs_stf) %>% 
  left_join(name_chapters) %>% 
  mutate(prop = dts/dts_tot)

# all causes trends ####
# ~~~~~~~~~~~~~~~~~~~~~~

bsn_tot <- 
  db_tot %>% 
  rename(dts = dts_tot,
         bts = bts_cdc) %>% 
  group_by(year) %>% 
  summarise(dts = sum(dts),
            bts = mean(bts)) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup()%>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_tot %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  # facet_wrap(~age, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/01_infant_trend.png")

bsn_age <- 
  db_tot %>% 
  rename(dts = dts_tot,
         bts = bts_cdc) %>% 
  group_by(age) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup()%>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_age %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(~age, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/02_infant_ages_trend.png")


write.excel(bsn_age)
write.excel(bsn_tot)




# by cause by age
bsn_chapters_age <- 
  db_chp %>% 
  rename(bts = bts_cdc) %>% 
  group_by(age, code) %>% 
  mutate(n = n()) %>% 
  filter(n == 6) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

write.excel(bsn_chapters_age)

bsn_chapters_age %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(age~name, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/03_infant_chapters_ages_trend.png",
       w = 12,
       h = 8)


test <- 
  bsn_chapters_age %>% 
  filter(year == 2020) %>% 
  mutate(dts_all = sum(dts),
         bsn_all = sum(bsn),
         exc = dts - bsn,
         exc_neg = ifelse(exc < 0,exc, 0),
         exc_pos = ifelse(exc > 0,exc, 0),
         exc_neg_all = sum(exc_neg),
         exc_pos_all = sum(exc_pos),
         cont_neg = exc_neg / sum(exc_neg),
         cont_pos = exc_pos / sum(exc_pos)) 

cause_cont <- 
  test %>% 
  group_by(cause) %>% 
  summarise(cont_neg = sum(cont_neg),
            cont_pos = sum(cont_pos))

cause_cont %>% 
  ggplot()+
  geom_bar(aes(x = cont_neg, y = cause), 
           stat = "identity", 
           position = "stack")

# by cause all ages
bsn_chapters <- 
  db_chp %>% 
  rename(bts = bts_cdc) %>% 
  group_by(code, name, year) %>% 
  summarise(dts = sum(dts),
            bts = mean(bts)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_chapters %>% 
  filter(name != "other") %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(~name, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/04_infant_chapters_trend.png",
       w = 10,
       h = 4)


# analysis of perinatal mortality ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
codes_peri <- 
  read_xlsx("Data/USA/infant_causes_summary.xlsx",
            sheet = "causes details") %>% 
  drop_na(code) %>% 
  filter(chapter == "GR130-070",
         level == 2) %>% 
  pull(code)

db_peri <- 
  db_css %>% 
  filter(code %in% codes_peri & code != "GR130-114")

bsn_peri <- 
  db_peri %>% 
  rename(bts = bts_cdc) %>% 
  group_by(code, year) %>% 
  summarise(dts = sum(dts),
            bts = mean(bts)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_peri %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(~code, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/05_infant_pericauses_trend.png")

bsn_peri_age <- 
  db_peri %>% 
  rename(bts = bts_cdc) %>% 
  group_by(age, code) %>% 
  # filter(n() == 6) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_peri_age %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(age~code, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/06_infant_pericause_age_trend.png")








# analysis of perinatal mortality disagg ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
codes_peri_cong_small <- 
  read_xlsx("Data/USA/infant_causes_summary.xlsx",
            sheet = "causes details") %>% 
  drop_na(code) %>% 
  filter(chapter %in% c("GR130-070", "GR130-118"),
         subdivided == 0) %>% 
  pull(code)

main_peri_cong <- 
  db_css %>% 
  filter(code %in% codes_peri_cong_small) %>% 
  group_by(age, code) %>% 
  summarise(dts = sum(dts)) %>% 
  group_by(age) %>% 
  mutate(prop = dts / sum(dts)) %>% 
  arrange(age, -prop) %>% 
  mutate(cont = 1:n()) %>%
  ungroup() %>%
  filter(cont <= 10) %>%
  # filter(prop >= 0.05) %>% 
  select(age, code, cont)

codes_per_cong_small <- 
  read_xlsx("Data/USA/infant_causes_summary.xlsx",
            sheet = "per_cong")

db_peri_cong_small <- 
  db_css %>% 
  left_join(main_peri_cong) %>% 
  filter(cont %in% 1:10) %>% 
  rename(bts = bts_cdc) %>% 
  left_join(codes_per_cong_small)

bsn_peri_cong_small <- 
  db_peri_cong_small %>% 
  group_by(age, code) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_peri_cong_small %>% 
  filter(cont <= 5) %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(age~name, scales = "free", ncol = 5)+
  theme_bw()
ggsave("Figures/causes_us/07_infant_peri_cong_detailed_trend.png",
       w = 12,
       h = 7)

exc_peri_cong <- 
  bsn_peri_cong_small %>% 
  mutate(pscore = dts / bsn,
         out = ifelse(dts > up_bsn | dts < lp_bsn, "1", "0"))


exc_peri_cong %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_point(aes(pscore, name, col = out))+
  scale_x_log10()+
  geom_text(aes(pscore, name, label = paste0(cont, "(", round(mx), ")")), hjust = -0.3, size = 3)+
  facet_wrap(~age, scales = "free")+
  scale_color_manual(values = c("black", "red"))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")
ggsave("Figures/causes_us/08_infant_peri_cong_detailed_excess.png",
       w = 12,
       h = 6)


unique(bsn_peri_small$code)

write.excel(bsn_peri_cong_small)

bsn_peri_age <- 
  db_peri %>% 
  rename(bts = bts_cdc) %>% 
  group_by(age, code) %>% 
  # filter(n() == 6) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_peri_age %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(age~code, scales = "free")+
  theme_bw()
ggsave("Figures/causes_us/06_infant_pericause_age_trend.png")


# ====
# test onthe effect of some causes
# Newborn affected by maternal factors and by complications of pregnancy, labor and delivery (P00-P04):  "GR130-071" 
# Disorders related to length of gestation and fetal malnutrition (P05-P08): "GR130-086"
# Other respiratory conditions originating in the perinatal period (P23-P28): "GR130-097"
# ====
causes_remove <- c("GR130-071", "GR130-086", "GR130-097")

db_remove <- 
  db_peri %>% 
  filter(code %in% causes_remove) %>% 
  select(year, age, dts_rmv = dts) %>% 
  group_by(year, age) %>% 
  summarise(dts_rmv = sum(dts_rmv)) %>% 
  ungroup()

db_test <- 
  db_tot %>% 
  left_join(db_remove) %>% 
  mutate(dts = dts_tot - dts_rmv) %>% 
  rename(bts = bts_cdc)

bsn_test <- 
  db_test %>% 
  group_by(age) %>% 
  do(est_baseline_inf(chunk = .)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5 * dts / bts,
         mx_bsn = 1e5 * bsn / bts,
         mx_lp_bsn = 1e5 * lp_bsn / bts,
         mx_up_bsn = 1e5 * up_bsn / bts)

bsn_test %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = mx_lp_bsn, ymax = mx_up_bsn), alpha = 0.3)+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx_bsn))+
  facet_wrap(~age, scales = "free")+
  theme_bw()

