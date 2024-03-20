rm (list = ls())
source("code/00_functions.R")

sum_source_sbs_neo <- function(db){
  sum_out <- 
    db %>% 
    filter(year >= 2015) %>% 
    group_by(code, measure) %>% 
    summarise(value_t = sum(value),
              years = n(),
              source = unique(source) %>% paste(collapse = ", "),
              has_21 = ifelse(max(year) == 2021, 1, 0)) %>% 
    ungroup() %>% 
    unique()  %>% 
    mutate(measure2 = case_when(measure %in% c("sbs_r", "sbs") ~ "sbs",
                                measure %in% c("neo_r", "neo") ~ "neo",
                                TRUE ~ measure),
           units_m = case_when(measure %in% c("sbs_r", "neo_r") ~ "rates",
                               measure %in% c("sbs", "neo", "inf", "pos") ~ "counts",
                               TRUE ~ "other"))
}

ccd <- read_rds("data_inter/neo_sbs_unicef.rds")
ann <- read_rds("data_inter/neo_sbs_annual_unicef.rds")
bra <- read_rds("data_inter/neo_sbs_brazil.rds")
mex <- read_rds("data_inter/neo_sbs_mexico.rds")
usa <- read_rds("data_inter/neo_sbs_usa.rds")
zaf <- read_rds("data_inter/neo_sbs_zaf.rds")
eur <- read_rds("data_inter/neo_sbs_eur.rds")
unpd <- read_rds("data_inter/neo_sbs_unpd.rds")
chn_bgd <- read_rds("data_inter/neo_sbs_chn_bgd.rds")
  
sum_ccd <- sum_source_sbs_neo(ccd)
sum_ann <- sum_source_sbs_neo(ann)
sum_bra <- sum_source_sbs_neo(bra)
sum_mex <- sum_source_sbs_neo(mex)
sum_usa <- sum_source_sbs_neo(usa)
sum_zaf <- sum_source_sbs_neo(zaf)
sum_eur <- sum_source_sbs_neo(eur)
sum_unpd <- sum_source_sbs_neo(unpd)
sum_chn_bgd <- sum_source_sbs_neo(chn_bgd)

comp_all <- 
  bind_rows(sum_ccd,
            sum_ann,
            sum_bra,
            sum_mex,
            sum_usa,
            sum_zaf,
            sum_eur,
            sum_unpd,
            sum_chn_bgd) %>% 
  arrange(measure, code) %>% 
  unique() %>% 
  mutate(value_t = round(value_t)) %>% 
  group_by(measure2, code) %>% 
  filter(has_21 == max(has_21)) %>% 
  filter((any(source == "unicef_ccd") & source == "unicef_ccd") | all(source != "unicef_ccd")) %>%
  filter(years == max(years)) %>%
  filter((any(source == "unicef_ann") & source == "unicef_ann") | all(source != "unicef_ann")) %>%
  filter((any(source == "eurs") & source == "eurs") | all(source != "eurs")) %>%
  filter((any(units_m == "counts") & units_m == "counts") | all(units_m != "counts")) %>%
  # filter((any(units_m %in% c("sbs", "neo")) & units_m %in% c("sbs", "neo")) | all(!units_m %in% c("sbs", "neo"))) %>%
  # group_by(measure2, country, units_m) %>% 
  # filter(value_t == max(value_t)) %>%  
  # unique() %>% 
  # filter(1:n() == n())
  mutate(best = ifelse(n() == 1, source, NA))

sel_all <- 
  comp_all %>% 
  select(code, measure, source)

out_all <- 
  bind_rows(ccd,
            ann,
            bra,
            mex,
            usa,
            zaf,
            eur,
            unpd,
            chn_bgd) %>% 
  select(-country) %>% 
  inner_join(sel_all) %>% 
  arrange(measure, code, year) %>% 
  mutate(exposure = ifelse(measure == "sbs", bts + value, bts),
         Sex = "t") %>% 
  rename(Code = code,
         Year = year,
         Age = measure,
         Deaths = value, 
         Births = bts,
         Source = source,
         Exposure = exposure) %>% 
  filter(Age %in% c("sbs", "sbs_r", "neo", "neo_r")) %>% 
  select(-measure2)

test <- 
  out_all %>% 
  group_by(Code, Year, Age) %>% 
  filter(n() > 1)


# removing countries with insufficient periods
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# minimum 3 periods in 2015-2019

inc_per_all <- 
  out_all %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Code, Sex, Age) %>% 
  summarise(pers = n()) %>% 
  ungroup() %>% 
  filter(pers >= 3) %>% 
  select(-pers)

out_all2 <- 
  out_all %>% 
  inner_join(inc_per_all)

write_rds(out_all2, "data_inter/neonatal_and_stillbirths_all_countries.rds")

