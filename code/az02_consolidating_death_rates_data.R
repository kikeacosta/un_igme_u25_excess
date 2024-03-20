rm (list = ls())
source("code/00_functions.R")

# China and Bangladesh data shared by UNICEF from Sample Vital Registration
cn_bd <- read_xlsx("data_input/unicef/chn_bgd/SVR_BGD CHN.xlsx")
cn <- read_xlsx("data_input/unicef/china_xlsx.xlsx")
bd <- read_xlsx("data_input/unicef/SRS 2020 Table 4.21.xlsx",
                skip = 1)
za <- read_xlsx("data_input/unicef/230425_south_africa_rates.xlsx",
                sheet = "qx")


# India
ind1 <- 
  read_xlsx("data_input/unicef/svr_india.xlsx") %>% 
  select(-region, -Label, -`Survey type`) %>% 
  rename(region = area) %>% 
  filter(indicator == "IMR",
         region == "National",
         Year >= 2010) %>% 
  select(-indicator, -region)

ind2 <- 
  read_csv("data_input/unicef/India_IMR_2020.csv") %>% 
  rename(region = 1) %>% 
  filter(region == "India") %>% 
  select(value = Total) %>% 
  mutate(Year = 2020)

ind <- 
  bind_rows(ind1, ind2) %>% 
  mutate(code = "IND",
         sex = "t",
         measure = "inf_tot") %>% 
  rename(rate = value) %>% 
  arrange(Year)


# China and Bangladesh
# ~~~~~~~~~~~~~~~~~~~~
cn_bd2 <- 
  cn_bd %>% 
  select(code = 1, Year = 10,
         neo_tot = 13, inf_tot = 14, chd_tot = 15) %>% 
  mutate(sex = "t") %>% 
  gather(neo_tot, inf_tot, chd_tot, key = measure, value = rate)

cn2 <- 
  cn %>% 
  gather(-Year, key = measure, value = rate) %>% 
  filter(Year %in% 2010:2014) %>% 
  # separate(measure, c("measure", "location")) %>% 
  # mutate(measure = recode(measure,
  #                         "chd" = "0_4")) %>% 
  mutate(code = "CHN",
         sex = "t")

unique(cn2$measure)

bd2 <- 
  bd %>% 
  select(Year,
         inf = 2,
         neo = 3,
         # pos = 4,
         chd = 5) %>% 
  gather(-Year, key = measure, value = rate) %>% 
  filter(Year %in% 2010:2014) %>% 
  # mutate(measure = recode(measure,
  #                         "chd" = "0_4",
  #                         "chd2" = "1_4")) %>% 
  mutate(code = "BGD",
         measure = paste(measure, "tot", sep = "_"),
         sex = "t")

za2 <- 
  za %>% 
  gather(-age, -sex, key = year, value = qx) %>% 
  mutate(year = year %>% as.double()) %>% 
  filter(year >= 2010) %>% 
  mutate(code = "ZAF",
         measure = case_when(age == 0 ~ "inf_tot",
                             age == 1 ~ "chd2_tot",
                             age == 5 ~ "5_tot",
                             age == 10 ~ "10_tot",
                             age == 15 ~ "15_tot",
                             age == 20 ~ "20_tot")) %>% 
  rename(Year = year,
         rate = qx) %>% 
  select(-age)

all <- 
  bind_rows(
    cn_bd2,
    cn2,
    bd2,
    za2,
    ind
  ) %>% 
  rename(Code = code, Sex = sex, Rate = rate) %>% 
  separate(measure, c("Age", "Region")) %>% 
  arrange(Code, Year, Age)
  
write_rds(all, "data_inter/annual_rates_pop_infant_child_regions.rds")

unique(all$Age)

inf <- 
  all %>% 
  filter(Age %in% c("inf", "chd2"),
         Region == "tot") %>% 
  mutate(Age = case_when(Age == "inf" ~ "0",
                         Age == "chd2" ~ "1"),
         Age = Age %>% as.double(),
         age_up = case_when(Age == 0 ~ 0,
                            Age == 1 ~ 4)) %>% 
  select(-Region) %>% 
  arrange(Code, Sex, Age, Year)
  
yng <- 
  all %>% 
  filter(Age %in% c("chd", "5", "10", "15", "20"),
         Region == "tot") %>% 
  mutate(Age = case_when(Age == "chd" ~ "0",
                         TRUE ~ Age),
         Age = Age %>% as.double()) %>% 
  select(-Region) %>% 
  mutate(age_up = case_when(Age == 0 ~ 4,
                            Age == 5 ~ 9,
                            Age == 10 ~ 14,
                            Age == 15 ~ 19,
                            Age == 20 ~ 24)) %>% 
  arrange(Code, Sex, Age, Year)

write_rds(inf, "data_inter/annual_rates_infant_child.rds")
write_rds(yng, "data_inter/annual_rates_5y_groups.rds")

    

