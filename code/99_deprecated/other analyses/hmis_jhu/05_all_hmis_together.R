rm (list = ls())
source("Code/00_functions.R")
library(readxl)


# African countries
# =================
{

# using long format file
afro_raw <-
  read_xlsx("Data/HMIS/Continuity of Essential Services - Mortality-longformat 2022-07-22.xlsx")

# # using excel tab file
#   cts <- c("Kenya", "Eswatini", 
#            "Burundi", "Uganda", 
#            "Malawi", "Zambia", 
#            "Zimbabwe", "Ethiopia", 
#            "Madagascar")
#   
#   temp <- list()
#   
#   for(i in 1:length(cts)){
#     temp[[i]] <- 
#       read_xlsx("Data/HMIS/Continuity of Essential Services - Mortality - KEN, SWZ, BDI, UGA, MWI, ZMB, ZWE-22-03-30.xlsx",
#                 sheet = cts[i]) %>% 
#       mutate(Country = cts[i])
#   }
#   afro_raw <-
#     temp %>% 
#     bind_rows() 


unique(afro_raw$Country)
unique(afro_raw$Section)
unique(afro_raw$`Indicator (Long)`)
unique(afro_raw$`Indicator (Short)`)
unique(afro_raw$Year)
unique(afro_raw$Month)
unique(afro_raw$Completeness)

# ~~~~~~~
# === previous script to open Excel files with multiple tabs
# temp[[i]] <- 
#   read_xlsx(here("Data", "HMIS", "Continuity of Essential Services - Mortality - KEN, BDI, SWZ, UGA, MWI, ZMB, ZWE-21-08-26.xlsx"),
#             sheet = cts[i]) %>% 
#   mutate(Country = cts[i])
# 
# cts <- c("Kenya", "Eswatini", "Burundi", "Uganda", "Malawi", "Zambia", "Zimbabwe")
# 
# temp <- list()
# 
# for(i in 1:length(cts)){
#   temp[[i]] <- 
#     read_xlsx(here("Data", "HMIS", "Continuity of Essential Services - Mortality - KEN, BDI, SWZ, UGA, MWI, ZMB, ZWE-21-08-26.xlsx"),
#               sheet = cts[i]) %>% 
#     mutate(Country = cts[i])
# }
# afro_raw <- 
#   temp %>% 
#   bind_rows()

indics <- c("Number of U5 child deaths" = "0_4",
            "Number of maternal deaths" = "mat",
            "Number of newborn deaths" = "neo",
            "Number of stillbirths" = "sbs",
            "Live Births" = "bts",
            "Facility Deliveries" = "dvs")

# unique(db_all$`Indicator (Short)`)

afro_all <-
  afro_raw %>% 
  rename(indic = `Indicator (Short)`) %>% 
  mutate(Month = match(Month, month.abb),
         Date = make_date(d = 15, m = Month, y = Year),
         indic = recode(indic,
                        !!!indics)) %>% 
  select(country = Country, date = Date, measure = indic, value = Value) %>% 
  drop_na(value) %>% 
  spread(measure, value) %>% 
  mutate(bts = ifelse(is.na(bts), dvs - sbs, bts),
         dvs = ifelse(is.na(dvs), bts + sbs, dvs)) %>% 
  mutate(sbs_r = sbs / dvs,
         neo_r = neo / bts) %>% 
  gather(-country, -date, key = measure, value = value) %>% 
  drop_na()

unique(afro_all$measure)
unique(afro_all$country)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
}
# ==========



# Other HMIS countries
# ====================
{
hmis_raw <- 
  read_xlsx("Data/HMIS/20220830_Covid analysis data_HMIS.xlsx",
            sheet = 1) 

hmis2 <- 
  hmis_raw %>% 
  select(country = whoname, year, month, 
         ndth, d0, d0_4, lb, sb_x28wks) %>% 
  mutate(dvs = lb + sb_x28wks,
         sbs_r = sb_x28wks / dvs,
         neo_r = ndth / lb) %>% 
  pivot_longer(!c(country, year, month), names_to = "measure", values_to = "value") %>% 
  mutate(measure = case_when(measure == "d0_4" ~ "0_4",
                             measure == "d0" ~ "inf",
                             measure == "ndth" ~ "neo",
                             measure == "lb" ~ "bts",
                             measure == "sb_x28wks" ~ "sbs",
                             TRUE ~ measure),
         date = make_date(d = 15, m = month, y = year)) %>% 
  drop_na(value) %>% 
  select(-year, -month)
}



# Mozambique
# ==========
{
bts <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                 sheet = 1)

sbs_mths <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                      sheet = 2,
                      skip = 1)

dts <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                 sheet = 3)

bts2 <- 
  bts %>% 
  filter(dm != "Total") %>% 
  separate(dm, c("year", "month"), sep = "m") %>% 
  mutate(date = make_date(d = 15, m = as.double(month), y = as.double(year))) %>% 
  select(date, everything(), -year, -month)

sbs <- 
  bts2 %>% 
  mutate(sbs_r = SB / LBSB) %>% 
  rename(sbs = SB, bts = LB) %>% 
  select(-LBSB) %>% 
  gather(-date, key = measure, value = value) %>% 
  mutate(country = "Mozambique")

dts2 <- 
  dts %>% 
  separate(YYMM, c("year", "month"), sep = "m") %>% 
  mutate(date = make_date(d = 15,
                          m = as.double(month), 
                          y = as.double(year)),
         inf = `<1M` + `1_11M`,
         `0_4` = `<1M` + `1_11M` + `12_59M`) %>% 
  drop_na() %>% 
  select(-year, -month) %>%
  gather(-date, key = measure, value = value) %>% 
  mutate(measure = case_when(measure == "<1M" ~ "neo",
                             measure == "1_11M" ~ "pos",
                             measure == "12_59M" ~ "1_4",
                             measure == "5_9Y" ~ "5_9",
                             measure == "10_14Y" ~ "10_14",
                             measure == "15_19Y" ~ "15_19",
                             measure == "20_24Y" ~ "20_24",
                             TRUE ~ measure)) %>% 
  mutate(country = "Mozambique")

neo_r <- 
  dts2 %>% 
  spread(measure, value) %>% 
  select(date, neo) %>% 
  left_join(sbs %>% 
              spread(measure, value)) %>% 
  mutate(neo_r = neo / bts,
         measure = "neo_r") %>% 
  select(country, date, measure, value = neo_r)

moz <- 
  bind_rows(sbs,
            dts2,
            neo_r)

unique(moz$measure) %>% sort()
}
# ================




# Nigeria
# =======

nga <- 
  read_xlsx("Data/HMIS/HMIS_data_Nigeria_2015_2021_all_data.xlsx")


unique(nga$Indicator)

indics <- c("Stillbirths" = "sbs",
            "Live births" = "bts",
            # several deliveries types
            "Total deliveries" = "dvs_t",
            "Deliveries" = "dvs",
            "Deliveries - Normal" = "dvs_n",
            "Deliveries - Assisted" = "dvs_a",
            "Deliveries - Caesarian Section" = "dvs_c",
            # not clear the difference between "neonatal" and "0-28d"
            "Neonatal deaths" = "neo1",
            "Deaths (by age) 0-28d" = "neo2",
            "Child deaths 1 to 59m" = "1m_4y",
            "Under-five deaths" = "0_4",
            "Deaths (by age) 29d-11m" = "pos",
            "Deaths (by age) 12-59 m" = "1_4",
            "Deaths (by age) 5-9yrs" = "5_9",
            "Deaths (by age) 10-19yrs" = "10_19",
            "Deaths (by age) 20+ yrs" = "20+",
            "Deaths (by age)" = "age_unk")
 
nga2 <-
  nga %>% 
  rename(indic = Indicator) %>% 
  mutate(Month = match(Month, month.abb),
         Date = make_date(d = 15, m = Month, y = Year),
         indic = recode(indic,
                        !!!indics)) %>% 
  select(country = Country, date = Date, measure = indic, value = value) %>% 
  spread(measure, value) %>% 
  mutate(neo_s1 = `0_4` - `1m_4y`,
         neo_s2 = `0_4` - `1_4` - pos,
         `1_4_2` = `1m_4y` - pos)

unique(nga2$measure)


# %>% 
#   spread(measure, value) %>% 
#   mutate(bts = dvs - sbs) %>% 
#   mutate(sbs_r = sbs / dvs,
#          neo_r = neo / bts) %>% 
#   gather(-country, -date, key = measure, value = value) %>% 
#   drop_na()

unique(afro_all$measure)
unique(afro_all$country)

# =====




# India
# =====
{
db <- read_csv("Data/HMIS/HMIS_data_India_byState_2015_2021.csv")

unique(db$Indicator)
unique(db$Region)
unique(db$Year) %>% sort

good_cov <- c("Maharashtra", "Tamil Nadu", "Kerala")
examps <- c("All India", "Maharashtra", "Tamil Nadu", "Kerala", "Delhi", 
            "Madhya Pradesh", "Uttar Pradesh", "West Bengal")

unique(db$Indicator)
# Dadra & Nagar Haveli and Daman & Diu were integrated somewhere in 2020 
# Ladakh only has information for 2020

db2 <- 
  db %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "inf",
                            "Neonatal deaths" = "neo", 
                            "Stillbirths" = "sbs",
                            "Facility deliveries" = "dvs_fct",
                            "Home deliveries" = "dvs_home",
                            "Live birth" = "bts",
                            "Postneonatal deaths" = "pos",
                            "Child deaths age 1 to 4" = "1_4",
                            "Maternal deaths" = "mat",
                            "Under-five deaths" = "0_4",
                            "Deaths 1 month to 5 years" = "1m_4y"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year),
         Region = ifelse(Region %in% c("Dadra & Nagar Haveli",
                                       "Daman & Diu", 
                                       "The Dadra And Nagar Haveli And Daman And Diu"), 
                         "D&NH&D&D",
                         Region)) %>% 
  filter(Region != 	"Ladakh") %>% 
  group_by(Region, date, Indicator) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

unique(db2$Region)

test <- 
  db2 %>% 
  spread(Indicator, value) %>% 
  mutate(dvs_est = bts + sbs,
         dvs_tot = dvs_fct + dvs_home) %>% 
  select(Region, date, bts, sbs, dvs_est, dvs_fct, dvs_home, dvs_tot)



india <- 
  db2 %>%
  filter(Indicator %in% c("bts",
                          "sbs",
                          "inf",
                          "neo",
                          "pos",
                          "1_4",
                          "mat")) %>% 
  spread(Indicator, value) %>% 
  mutate(neo_r = neo / bts,
         sbs_r = sbs / (sbs + bts),
         `0_4` = inf + `1_4`) %>% 
  gather(-Region, -date, key = measure, value = value) %>% 
  drop_na() %>% 
  filter(Region == "All India") %>% 
  mutate(country = "India") %>% 
  select(-Region)

unique(india$measure)
}
# ==========




# Bangladesh
# ==========
{
db_raw <- read_xlsx("Data/HMIS/HMIS_data_Bangladesh_2017_2021.xlsx")

db_har <- 
  db_raw %>% 
  filter(Source == "Community Server - Health Assistant Report") %>% 
  select(Indicator, Year, Month, value)


db2 <- 
  db_har %>% 
  mutate(measure = recode(Indicator,
                            "Stillbirths" = "sbs",
                            "Early neonatal deaths" = "neo_ear", 
                            "Late neonatal deaths" = "neo_lat", 
                            "Postneonatal deaths" = "pos",
                            "Child deaths age 1 to 4" = "1_4",
                            "Maternal deaths" = "mat",
                            "Live birth" = "bts",
                            "Total deliveries" = "dvs",
                            "Infant deaths" = "inf",
                            "Neonatal deaths" = "neo", 
                            "Under-five deaths" = "0_4"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year)) %>% 
  select(date, measure, value)

ban_r <- 
  db2 %>% 
  filter(measure %in% c("sbs", "neo", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(sbs_r = sbs / (sbs + bts),
         neo_r = neo / bts) %>% 
  select(date, sbs_r, neo_r) %>% 
  gather(-date, key = measure, value = value)
  
ban <- 
  bind_rows(ban_r,
            db2) %>% 
  mutate(country = "Bangladesh")
}
# ==========================



# Merging all HMIS ====
# ~~~~~~~~~~~~~~~~~~~~~
{
all_cts <- 
  bind_rows(ban,
            moz,
            india,
            afro_all,
            hmis2)

unique(all_cts$country)

cts_lvs <- 
  c("Liberia",
    "Burkina Faso",
    "Gambia",
    "Ethiopia",
    "Kenya", 
    "Uganda", 
    "Burundi", 
    "Zambia", 
    "Malawi", 
    "Zimbabwe",
    "Mozambique",
    "Eswatini", 
    "Madagascar",
    "Afghanistan",
    "Bangladesh",
    "India")

all_cts <- 
  bind_rows(ban,
            moz,
            india,
            afro_all,
            hmis2)  %>% 
  # filter(country != "Eswatini") %>% 
  mutate(country = factor(country, levels = cts_lvs)) %>% 
  # Gambia has annual data and only since 2018: not enough observations
  filter(country != "Gambia")

}

write_rds(all_cts, "Output/hmis_all_countries.rds")

