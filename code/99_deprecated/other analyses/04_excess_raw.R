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

db <- 
  read_rds("Output/annual_deaths_pop.rds") %>% 
  filter(!Country %in% excl) %>% 
  mutate(Year = as.character(Year) %>% as.integer())

unique(db$Country)


# Identifying countries with data for each age group
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cts_inf <- 
  db %>% 
  filter(Age == 1,
         Year == 2020) %>% 
  pull(Country) %>% 
  unique()

cts_child <- 
  db %>% 
  filter(!Country %in% cts_inf,
         Age == 5,
         Year == 2020,
         Sex == "t") %>% 
  pull(Country) %>% 
  unique()

cts_5_9 <- 
  db %>% 
  filter(Age == 5 | Age == 10,
         Year == 2020,
         Sex == "t") %>% 
  group_by(Country) %>% 
  filter(n() == 2) %>% 
  pull(Country) %>% 
  unique()

cts_10_14 <- 
  db %>% 
  filter(Age == 10 | Age == 15,
         Year == 2020,
         Sex == "t") %>% 
  group_by(Country) %>% 
  filter(n() == 2) %>% 
  pull(Country) %>% 
  unique()

cts_15_24 <- 
  db %>% 
  filter(Age == 15 | Age == 25,
         Year == 2020,
         Sex == "t") %>% 
  group_by(Country) %>% 
  filter(n() == 2) %>% 
  pull(Country) %>% 
  unique()


# building databases by age
# ~~~~~~~~~~~~~~~~~~~~~~~~~

db_inf <- 
  db %>% 
  filter(Country %in% cts_inf,
         Age == 0)

db_1_4 <- 
  db %>% 
  filter(Country %in% cts_inf,
         Age %in% 1:4) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

db_child <- 
  db %>% 
  filter(Age %in% 0:4) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

db_5_9 <- 
  db %>% 
  filter(Country %in% cts_5_9,
         Age %in% 5:9) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

db_10_14 <- 
  db %>% 
  filter(Country %in% cts_10_14,
         Age %in% 10:14) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

db_15_24 <- 
  db %>% 
  filter(Country %in% cts_15_24,
         Age %in% 15:24) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

write.excel(db_1_4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


db_inf_ecx <- est_exc(db_inf, "Infant")
db_1_4_ecx <- est_exc(db_1_4, "1_4")
db_child_ecx <- est_exc(db_child, "Child (0-4)")
db_5_9_ecx <- est_exc(db_5_9, "5_9")
db_10_14_ecx <- est_exc(db_10_14, "10_14")
db_15_24_ecx <- est_exc(db_15_24, "15_24")

db_all_fit <- 
  bind_rows(db_inf_ecx,
            db_1_4_ecx,
            db_child_ecx,
            db_5_9_ecx,
            db_10_14_ecx,
            db_15_24_ecx)

write_rds(db_all_fit, "Output/p_scores_excess_raw_data.rds")

# Plotting some examples of fitting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
age_lvs <- c("Infant", "1_4", "Child (0-4)", "5_9", "10_14", "15_24")
cts <- c("Uruguay", "USA", "Israel", "Brazil")

chunk_plot <- 
  db_all_fit %>% 
  filter(Country %in% cts,
         Age != "Child (0-4)",
         Sex == "t") %>% 
  mutate(is_2020 = ifelse(Year == 2020, "y", "n"),
         Age = factor(Age, levels = age_lvs))

tx <- 5
chunk_plot %>% 
  ggplot()+
  geom_point(aes(Year, Deaths, col = is_2020, size = is_2020))+
  geom_ribbon(aes(Year, ymin = lp_baseline, ymax = up_baseline), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(Year, Baseline))+
  scale_color_manual(values = c("black", "red"))+
  scale_size_manual(values = c(1, 1.5))+
  facet_wrap(Country ~ Age, scales = "free", ncol = 5)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1),
                                    size = tx + 3),
        axis.text = element_text(size = tx),
        strip.background = element_rect(fill = "transparent"))

# ggsave("Figures/excess_raw/example_fitting.png")

cts <- c("USA")

chunk_plot <- 
  db_all_fit %>% 
  filter(Country %in% cts,
         Age == "1_4",
         Sex != "t") %>% 
  mutate(is_2020 = ifelse(Year == 2020, "y", "n"),
         Age = factor(Age, levels = age_lvs))

chunk_plot %>% 
  ggplot()+
  geom_point(aes(Year, Deaths, col = is_2020, size = is_2020))+
  geom_ribbon(aes(Year, ymin = lp_baseline, ymax = up_baseline), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(Year, Baseline))+
  scale_color_manual(values = c("black", "red"))+
  scale_size_manual(values = c(1, 1.5))+
  facet_wrap( ~ Sex, scales = "free", ncol = 1)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1),
                                    size = tx + 3),
        axis.text = element_text(size = tx),
        strip.background = element_rect(fill = "transparent"))

ggsave("Figures/excess_raw/example_fitting_USA.png")
