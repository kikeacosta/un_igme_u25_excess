rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
dts_fit <- 
  read_rds("Output/p_scores_excess_deaths.rds") %>% 
  select(Country, Code, Income, Year, Sex, Age, Population, Deaths, Rate,
         bsn, bsn_lp, bsn_up, psc, up, lp, Source)

rts_fit <- 
  read_rds("Output/p_scores_excess_rates.rds") %>% 
  select(Country, Code, Income, Year, Sex, Age, Population, Rate, 
         bsn, bsn_lp, bsn_up, psc, up, lp, Source)


# merging deaths and rates baselines and p-scores
all_fit <- 
  bind_rows(dts_fit, rts_fit) %>% 
  arrange(Country, Code, Age, Sex, Year)

# quick summary
all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())

# excluding groups with fitting issues
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# countries to exclude from results because of lack of confidence in data quality 
exc1 <- c("Kenya")

# identifying issues with baseline fitting
exc2 <- 
  all_fit %>% 
  filter(Year %in% 2020:2021) %>% 
  filter(psc == 0 | is.na(psc) | is.na(lp) | is.na(up) |
           lp == Inf | up == Inf) %>% 
  select(Country, Sex, Age) %>% 
  unique() %>% 
  mutate(exc = 1) %>% 
  arrange(Country, Sex, Age)

# summary of issues
sum_exc <- 
  exc2 %>% 
  group_by(Country, Sex) %>% 
  summarise(Ages = unique(Age) %>% paste(collapse = ", "))

exc_sex_t <- 
  sum_exc %>% 
  filter(Sex == "t") %>% 
  select(-Sex)

pop_cts <- 
  all_fit %>% 
  filter(Sex == "t",
         Year == 2020) %>% 
  group_by(Country) %>% 
  summarise(pop = sum(Population))

av_pop <- read_rds("data_inter/average_observed_population_0_24.rds") %>% pull(av_pop)

exc_sex_t2 <- 
  exc_sex_t %>% 
  left_join(pop_cts) %>% 
  mutate(rel_av = 100 * round(pop / av_pop, 4))

write.excel(exc_sex_t2)

ll_0 <- 
  all_fit %>% 
  filter(Year %in% 2020:2021) %>% 
  filter(bsn_lp == 0) %>% 
  arrange(Country, Age, Year)

# excluding cases with issues
all_fit2 <- 
  all_fit %>% 
  left_join(exc2) %>% 
  filter(is.na(exc)) %>% 
  select(-exc) %>% 
  filter(!Country %in% exc1)


# quick summary after excluding issues
summ <- 
  all_fit2 %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())
summ

write.excel(summ)

all_fit2 %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n())



# saving merged estimates
write_rds(all_fit2, "Output/p_scores_excess_deaths_rates.rds")


