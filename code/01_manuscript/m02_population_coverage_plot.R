rm(list = ls())
source("code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
crvs <- 
  read_rds("data_output/preliminary_pscores_2024-01-16.rds") %>% 
  filter(Year %in% 2020:2021,
         inc_in == 1,
         Sex == "t") %>% 
  select(code = Code, year = Year, age = Age)

unique(crvs$age)

hmis <- 
  read_rds("data_inter/hmis_annual_baselines.rds") %>% 
  filter(year %in% 2020:2021) %>% 
  mutate(code = countrycode(country, origin = "country.name",
                            destination = "iso3c")) %>% 
  select(code, year, age = measure) %>% 
  mutate(age = factor(age, levels = c("Stillbirths",
                                      "Neonatal",
                                      "Infant",
                                      "1-4",
                                      "0-4",
                                      "5-9",
                                      "10-14",
                                      "15-19",
                                      "20-24")))

all_dts <- 
  bind_rows(crvs, hmis) %>% 
  unique() %>% 
  mutate(inc = 1)


income_levels <- read_xlsx("data_input/WPP2022_F01_LOCATIONS.XLSX") %>% 
  select(code = 5, type = 9, region = 19, 
         hi = 27, umi = 29, lmi = 30, li = 31) %>% 
  filter(type == "Country/Area") %>% 
  mutate(income = case_when(!is.na(hi) ~ "High",
                            !is.na(umi) ~ "Upper-mid",
                            !is.na(lmi) ~ "Lower-mid",
                            !is.na(li) ~ "Low",
                            TRUE ~ "No income data")) %>% 
  select(-hi, -umi, -lmi, -li)

incs <- c("Low", "Lower-mid", "Upper-mid", "High")

# population and births data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_rds("data_inter/exposures_5-year_groups.rds") %>% 
  filter(Sex == "t",
         Year %in% 2020:2021) %>% 
  select(code = Code, year = Year, age = Age, pop = Population)

pop_chd <- 
  pop %>% 
  filter(age <= 1) %>% 
  summarise(pop = sum(pop), .by = c(code, year)) %>% 
  mutate(age = "0-4")

pop_rest <- 
  pop %>% 
  filter(age <= 24) %>% 
  mutate(age = case_when(age == 0 ~ "Infant",
                         age == 1 ~ "1-4",
                         age == 5 ~ "5-9",
                         age == 10 ~ "10-14",
                         age == 15 ~ "15-19",
                         age == 20 ~ "20-24"))

pop_neo_stb <- 
  bind_rows(pop_rest %>% filter(age == "Infant") %>% mutate(age = "Neonatal"),
            pop_rest %>% filter(age == "Infant") %>% mutate(age = "Stillbirths"))


pop_all <- 
  bind_rows(pop_chd,
            pop_rest,
            pop_neo_stb) %>% 
  mutate(age = factor(age, levels = c("Stillbirths",
                                      "Neonatal",
                                      "Infant",
                                      "1-4",
                                      "0-4",
                                      "5-9",
                                      "10-14",
                                      "15-19",
                                      "20-24"))) %>% 
  left_join(income_levels) %>% 
  unique()


# merging population and countries included in the analyses 
dt <- 
  pop_all %>% 
  left_join(all_dts) %>% 
  replace_na(list(inc = 0)) %>% 
  mutate(pop_inc = pop*inc) %>% 
  filter(income %in% incs) %>% 
  mutate(income = factor(income, levels = c(incs)))
  
unique(dt$income)

# calculating coverages
# ~~~~~~~~~~~~~~~~~~~~~

# by age
c_age <- 
  dt %>% 
  summarise(pop = sum(pop),
            pop_inc = sum(pop_inc), 
            .by = c(year, age)) %>% 
  mutate(cov = pop_inc/pop)

# by income
c_age_inc <- 
  dt %>% 
  summarise(pop = sum(pop),
            pop_inc = sum(pop_inc), 
            .by = c(year, age, income)) %>% 
  mutate(cov = pop_inc/pop)

# by region
c_age_reg <- 
  dt %>% 
  summarise(pop = sum(pop),
            pop_inc = sum(pop_inc), 
            .by = c(year, age, region)) %>% 
  mutate(cov = pop_inc/pop)

# plots ====
# ~~~~~~~~~~
c_age %>% 
  ggplot()+
  geom_point(aes(cov, age))+
  facet_grid(~year)+
  theme_bw()

c_age_inc %>% 
  ggplot()+
  geom_point(aes(cov, age))+
  facet_grid(year~income)+
  theme_bw()

bks <- seq(0, 1, 0.25)
lbs <- bks*100

c_age_inc %>% 
  filter(!(age %in% c("Infant", "1-4"))) %>% 
  ggplot(aes(cov, age))+
  geom_bar(stat = "identity", fill = "black")+
  geom_text(aes(label = paste0(round(cov*100)), group = income), 
            position = position_stack(vjust = 0.7, reverse = F), 
            size = 2,
            col = "white")+
  scale_x_continuous(breaks = bks,
                     labels = lbs)+
  facet_grid(year~income)+
  labs(y = "Age", x = "Covered global population (%)")+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
  )
  
ggsave("figures/01_manuscript/fig01_coverage_income.png", 
       dpi = 700,
       width = 7, height = 3)


unique(c_age_reg$region)
c_age_reg %>% 
  filter(!(age %in% c("Infant", "1-4"))) %>% 
  mutate(region = ifelse(region == "Latin America and the Caribbean", 
                         "LAC", 
                         region)) %>% 
  ggplot(aes(cov, age))+
  geom_bar(stat = "identity", fill = "black")+
  geom_text(aes(label = paste0(round(cov*100)), group = region), 
            position = position_stack(vjust = 0.7, reverse = F), 
            size = 2,
            col = "white")+
  scale_x_continuous(breaks = bks,
                     labels = lbs)+
  facet_grid(year~region)+
  labs(y = "Age", x = "Covered global population (%)")+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
  )

ggsave("figures/01_manuscript/figS05_coverage_region.png", 
       dpi = 700,
       width = 7, height = 3)



pop_tot_inc <- 
  pop_all %>% 
  summarise(pop = sum(pop), .by = c(year, age, income)) %>% 
  drop_na

pop_tot_reg <- 
  pop_all %>% 
  summarise(pop = sum(pop), .by = c(year, age, region)) %>% 
  drop_na

pop_tot <- 
  pop_all %>% 
  summarise(pop = sum(pop), .by = c(year, age)) %>% 
  drop_na


