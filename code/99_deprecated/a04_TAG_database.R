library(here)
source("Code/00_functions.R")
library(readxl)

group5 <- function(db){
  db %>% 
    mutate(Age = Age - Age %% 5) %>% 
    group_by(Country, Code, Year, Sex, Age, Source) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    group_by(Code, Year, Sex) %>% 
    mutate(age_up = case_when(last(Age) == 20 & Age == 20 ~ 24,
                              last(Age) == 0 & Age == 0 ~ 4,
                              TRUE ~ lead(Age - 1))) %>% 
    ungroup()
}

# last version of TAG in the website, William estimates
# https://msemburi.shinyapps.io/excessvis/

tag_will <- 
  read_xlsx("Data/TAG/220411_Observed deaths by sex and age.xlsx",
            skip = 5)

unique(tag_will$age)

tag_will2 <- 
  tag_will %>% 
  rename(code = iso3,
         Deaths = val) %>% 
  group_by(Country, year, sex) %>% 
  arrange(Country, year, sex, age) %>% 
  mutate(age_up = lead(age - 1)) %>% 
  ungroup() %>% 
  filter(age <= 24 & age_up <= 25) %>% 
  mutate(age = case_when(age >= 1 & age_up <= 4 ~ 1,
                         age >= 5 & age_up <= 9 ~ 5,
                         age >= 10 & age_up <= 14 ~ 10,
                         age >= 15 & age_up <= 19 ~ 15,
                         age >= 20 ~ 20,
                         TRUE ~ age),
         sex = recode(sex,
                      "Female" = "f",
                      "Male" = "m")) %>% 
  group_by(Country, code, year, sex, age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Country = recode(Country,
                          "Czech Republic" = "Czechia",
                          "Iran (Islamic Republic of)" = "Iran",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "United States of America" = "USA",
                          "Republic of Korea" = "South Korea",
                          "Russian Federation" = "Russia"))

# adding all sex
tag_will3 <- 
  tag_will2 %>% 
  group_by(code, Country, age, year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(sex = "t") %>% 
  bind_rows(tag_will2) %>% 
  select(Code = code, 
         Country,
         Year = year,
         Sex = sex,
         Age = age,
         dts_will = Deaths)
    
unique(tag_will3$Country)
unique(tag_will3$Sex)


# current version of data from all
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_our <- read_rds("Output/annual_young_deaths_all_countries.rds")

our_g5 <- group5(dts_our)

all_tag <- 
  our_g5 %>% 
  rename(dts_our = Deaths) %>% 
  full_join(tag_will3)  %>% 
  mutate(diff = dts_will / dts_our) %>% 
  select(Country, Code, Year, Sex, Age, age_up, everything()) %>% 
  filter(Year >= 2020)

all_ages_tag <- 
  all_tag %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(dts_our = sum(dts_our),
            dts_will = sum(dts_will)) %>% 
  ungroup() %>% 
  mutate(ratio = round(dts_will / dts_our, 2)) %>% 
  filter(Sex == "t")



all_ages_tag %>% 
  ggplot()+
  geom_point(aes(ratio, Country, col = as.factor(Year)))+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_wrap(~Year)+
  theme_bw()





# 
# # # current version of data from TAG
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tag_orig <- 
#   read_csv("Data/annual_deaths_countries_selected_sources_young.csv")
# 
# tag_orig2 <- 
#   tag_orig %>% 
#   # filter(Source %in% c("who", "unpd")) %>% 
#   group_by(Country, Year, Sex) %>% 
#   arrange(Country, Year, Sex, Age) %>% 
#   mutate(age_up = lead(Age - 1)) %>% 
#   filter(Age <= 24 & age_up <= 25) %>% 
#   ungroup() %>% 
#   mutate(Age = case_when(Age >= 1 & age_up <= 4 ~ 1,
#                          Age >= 5 & age_up <= 9 ~ 5,
#                          Age >= 10 & age_up <= 14 ~ 10,
#                          Age >= 15 & age_up <= 19 ~ 15,
#                          Age >= 20 ~ 20,
#                          TRUE ~ Age),
#          Deaths = round(Deaths)) %>% 
#   group_by(Country, Code, Year, Sex, Age, Source) %>% 
#   summarise(Deaths = sum(Deaths)) %>% 
#   ungroup() %>% 
#   mutate(Country = recode(Country,
#                           "Czech Republic" = "Czechia")) %>% 
#   group_by(Country, Sex, Year) %>% 
#   mutate(age_up = ifelse(Age == 20, 24, lead(Age - 1))) %>% 
#   ungroup() 
# 
# 
# tag_orig_g5 <- group5(tag_orig2)
# 
# # comparing both sources
# 
# all_tag <- 
#   tag_orig_g5 %>% 
#   rename(dts_orig = Deaths) %>% 
#   inner_join(tag_will2 %>% 
#                select(Code = code, 
#                       Year = year,
#                       Sex = sex,
#                       Age = age,
#                       dts_will = Deaths))  %>% 
#   mutate(diff = dts_will / dts_orig) %>% 
#   select(Country, Code, Year, Sex, Age, age_up, everything())
# 
# all_tag <- 
#   tag_will2 %>% 
#   select(Code = code, 
#          Country,
#          Year = year,
#          Sex = sex,
#          Age = age,
#          dts_will = Deaths) %>% 
#   left_join(tag_orig_g5 %>% 
#               select(Country, Year, Sex, Age, dts_orig = Deaths)) %>% 
#   mutate(diff = dts_will / dts_orig)
# 
# 
# 
# all_tag2 <- 
#   all_tag %>% 
#   group_by(Country, Year) %>% 
#   summarise(dts_will = sum(dts_will),
#             dts_orig = sum(dts_orig))


