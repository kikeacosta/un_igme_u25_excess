rm (list = ls())
source("code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~
pop_wpp <- 
  read_rds("data_inter/wpp_populations_ages_0_24_v2022.rds") %>% 
  select(Code = code, Year = year, Sex = sex, Age = age, Pop = pop) %>% 
  filter(Age <= 24,
         Year %in% 2020:2022) %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age %in% 1:4 ~ 1,
                         Age >= 5 ~ Age - Age%%5)) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Population = sum(Pop)) %>% 
  ungroup()

# Population for infant and child mortality
pop_inf_chl <- 
  pop_wpp %>% 
  filter(Age %in% 0:1) %>% 
  mutate(Age = recode(Age,
                      "0" = "Infant",
                      "1" = "1-4"))

# Population for 5-year age groups
pop_5y_grps <- 
  pop_wpp %>% 
  mutate(Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Age = recode(Age,
                      "0" = "0-4",
                      "5" = "5-9",
                      "10" = "10-14",
                      "15" = "15-19",
                      "20" = "20-24"))

# merging income data
# ~~~~~~~~~~~~~~~~~~~
income_levels <- read_xlsx("data_input/WPP2022_F01_LOCATIONS.XLSX") %>% 
  select(Code_join = 5, type = 9, Region = 13, 
         hi = 27, umi = 29, lmi = 30, li = 31) %>% 
  filter(type == "Country/Area") %>% 
  mutate(Income = case_when(!is.na(hi) ~ "High",
                            !is.na(umi) ~ "Upper-mid",
                            !is.na(lmi) ~ "Lower-mid",
                            !is.na(li) ~ "Low",
                            TRUE ~ "No income data"),
         Code_join = ifelse(Code_join == "XKX", "RKS", Code_join)) %>% 
  select(-hi, -umi, -lmi, -li)

unique(income_levels$Income)

income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low", "No income data")

pop_inc <- 
  pop_5y_grps %>% 
  bind_rows(pop_inf_chl) %>% 
  mutate(Code_join = Code) %>%
  left_join(income_levels, by = "Code_join") %>% 
  # Imputing High-income level for Montserrat-MSR (French territory) and 
  # Anguilla-AIA (UK territory), as for other overseas territories (French Polynesia, 
  # Guadeloupe, )
  mutate(Income = ifelse(Code %in% c("AIA", "MSR"), "High", Income)) %>% 
  select(-Code_join, -type) %>% 
  filter(Income %in% c("High", "Upper-mid", "Lower-mid", "Low")) %>%
  rename(pop_tot = Population) %>%
  summarise(pop_tot = sum(pop_tot), .by = c(Year, Income, Sex, Age))

unique(pop_inc$Income)

pop_inc_all <- 
  pop_inc %>% 
  bind_rows(
    pop_inc %>% 
      summarise(pop_tot = sum(pop_tot), .by = c(Year, Sex, Age)) %>%
      mutate(Income = "Total")
  ) %>% 
  # bind_rows(pop_inf_chl) %>% 
  mutate(Income = factor(Income, levels = income_lvs))

write_rds(pop_inc_all, "data_inter/population_by_income_2022.rds")

