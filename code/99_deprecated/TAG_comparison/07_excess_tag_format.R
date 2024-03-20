library(here)
source(here("Code", "00_functions.R"))

# populations to exclude from the analyses
excl <- c(
  # too small population
  "French Polynesia", 
  "Seychelles", 
  "Iceland", 
  "Montenegro",
  "Luxembourg",
  "Malta",
  "Cyprus",
  # too poor age configuration
  "Australia",
  "Germany",
  "China, Macao SAR",
  "South Korea"
)

excl <- c()
  
db <- 
  read_rds("Output/annual_deaths_pop.rds") %>% 
  filter(!Country %in% excl) %>% 
  mutate(Year = as.character(Year) %>% as.integer())

unique(db$Country)
unique(db$Age)


db2 <- 
  db %>% 
  mutate(Age = floor(Age / 5) * 5) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup() %>% 
  group_by(Country, Year, Sex) %>% 
  mutate(upper_age = lead(Age) - 1)


# Identifying countries with data for each age group
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db3 <- 
  db2 %>% 
  filter(
    Age == 0 & upper_age == 4 |
      Age == 5 & upper_age == 9 |
      Age == 10 & upper_age == 14 |
      Age == 15 & upper_age == 19 |
      Age == 20)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_all_fit <- 
  db3 %>% 
  group_by(Country, Sex, Age) %>% 
  do(est_baseline(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(p_score = Deaths / Baseline,
         up = Deaths / lp_baseline,
         lp = Deaths / up_baseline)


write_rds(db_all_fit, "Output/p_scores_excess_tag_format.rds")
