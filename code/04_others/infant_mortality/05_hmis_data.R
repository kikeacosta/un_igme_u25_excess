library(here)
source(here("Code", "00_functions.R"))
library(readxl)

# Health Information System (HMIS) data for 5 countries in Sub-Saharan Africa.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of stillbirths: # of Babies born with no sign of life and weighing at least 1000g or after 28 weeks gestation
# Number of newborn deaths: # Newborns who die in the first 28 days of life
# Number of U5 child deaths:	# of Children 1-59 months who died
# Number of maternal deaths:	# of Women who die either while pregnant or within the first 42 days of the end of pregnancy
# Facility Deliveries:	# of facility deliveries (Number of delivieries in facility)


cts <- c("Kenya", "Eswatini", "Burundi", "Uganda", "Malawi", "Zambia", "Zimbabwe")

temp <- list()

for(i in 1:5){
  temp[[i]] <- 
    read_xlsx(here("Data", "HMIS", "Continuity of Essential Services - Mortality - KEN, BDI, SWZ, UGA, MWI, ZMB, ZWE-21-08-26.xlsx"),
              sheet = cts[i]) %>% 
    mutate(Country = cts[i])
}

indics <- c("Number of U5 child deaths" = "child_deaths",
            "Number of maternal deaths" = "d_maternal",
            "Number of newborn deaths" = "neonatal_deaths",
            "Number of stillbirths" = "stillbirths", 
            "Facility Deliveries" = "deliveries")

db_all <-
  temp %>% 
  bind_rows() %>% 
  rename(indic = `Indicator (Short)`) %>% 
  mutate(Month = match(Month, month.abb),
         Date = make_date(d = 15, m = Month, y = Year),
         indic = recode(indic,
                        !!!indics)) %>% 
  select(-'Indicator (Long)', -Section, -Completeness)

# unique(db_all$indic)

# Early infancy death rates
# ~~~~~~~~~~~~~~~~~~~~~~~~~
births <- 
  db_all %>% 
  filter(indic == 'deliveries') %>% 
  select(Country, Date, Year, Quarter, Month, Births = Value)

stillb <- 
  db_all %>% 
  filter(indic == 'stillbirths') %>% 
  select(Country, Date, Year, Quarter, Month, Stills = Value)

neo <- 
  db_all %>% 
  filter(indic == 'neonatal_deaths') %>% 
  select(Country, Date, Year, Quarter, Month, Neo_deaths = Value)

# Perinatal rates (excluding those aged < 1 week of age)
peri_rate <- 
  births %>% 
  left_join(stillb) %>% 
  mutate(Value = 1000 * Stills / (Stills + Births),
         indic = "fetal_rates") %>% 
  select(Country, Date, Year, Quarter, Month, indic, Value)

# Neonatal mortality rates
neo_rate <- 
  births %>% 
  left_join(neo) %>% 
  mutate(Value = 1000 * Neo_deaths / Births,
         indic = "neonatal_rates") %>% 
  select(Country, Date, Year, Quarter, Month, indic, Value)

# Perinatal + Neonatal mortality rates
peri_neo_rate <- 
  births %>% 
  left_join(neo) %>% 
  left_join(stillb) %>% 
  mutate(Value = 1000 * (Neo_deaths + Stills) / (Births + Stills),
         indic = "peri_neonatal_rates") %>% 
  select(Country, Date, Year, Quarter, Month, indic, Value)

db_all2 <- 
  db_all %>% 
  bind_rows(peri_rate,
            neo_rate,
            peri_neo_rate)

# Plots of each indicators by country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Monthly
plot_this <- function(db, indr){
  db %>%
    filter(indic %in% indr) %>% 
    ggplot()+
    geom_line(aes(Date, Value))+
    facet_wrap(~Country, scale = "free")+
    labs(title = indr)+
    theme_bw()
}

plot_this(db_all2, "neonatal_deaths")
plot_this(db_all2, "deliveries")
plot_this(db_all2, "neonatal_rates")
plot_this(db_all2, "fetal_rates")
plot_this(db_all2, "peri_neonatal_rates")

plot_this(db_all2, "child_deaths")
plot_this(db_all2, "stillbirths")
plot_this(db_all2, "d_maternal")


db_all2 %>% 
  filter(indic %in% c("neonatal_deaths", 
                      "stillbirths")) %>% 
  ggplot()+
  geom_line(aes(Date, Value, col = indic))+
  facet_wrap(~Country, scale = "free")+
  # labs(title = indr)+
  theme_bw()


# comparative plots
# ~~~~~~~~~~~~~~~~~

# Normalizing values relative to the average
db_all3 <- 
  db_all2 %>% 
  group_by(Country, indic) %>% 
  mutate(av_val = mean(Value),
         std_val = Value / av_val) %>% 
  ungroup()


# children deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~
db_all3 %>% 
  filter(indic %in% c("neonatal_deaths", 
                      "stillbirths",
                      "child_deaths",
                      "deliveries")) %>% 
  mutate(indic = factor(indic, levels = c("stillbirths", 
                                          "neonatal_deaths",
                                          "child_deaths",
                                          "deliveries"))) %>% 
  ggplot()+
  geom_line(aes(Date, Value))+
  facet_grid(indic ~ Country, scale = "free")+
  scale_x_date(date_labels = "%yy")+
  # labs(title = indr)+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 7)
  )

ggsave("Figures/infant/hmis_child_counts.png", width = 6, height = 5)

db_all3 %>% 
  filter(indic %in% c("neonatal_deaths", 
                      "stillbirths",
                      "child_deaths",
                      "deliveries")) %>% 
  mutate(indic = factor(indic, levels = c("stillbirths", 
                                          "neonatal_deaths",
                                          "child_deaths",
                                          "deliveries"))) %>% 
  ggplot()+
  geom_line(aes(Date, std_val))+
  facet_grid(indic ~ Country, scale = "free")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_x_date(date_labels = "%yy")+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

ggsave("Figures/infant/hmis_child_normalized.png", width = 6, height = 5)

# 
# # births and infant deaths
# # ~~~~~~~~~~~~~~~~~~~~~~~~
# db_all3 %>% 
#   filter(indic %in% c("neonatal_deaths", 
#                       "stillbirths",
#                       "deliveries")) %>% 
#   mutate(indic = factor(indic, levels = c("deliveries",
#                                           "stillbirths", 
#                                           "neonatal_deaths"))) %>% 
#   ggplot()+
#   geom_line(aes(Date, Value))+
#   facet_grid(indic ~ Country, scale = "free")+
#   # labs(title = indr)+
#   theme_bw()+
#   theme(
#     legend.position = "bottom"
#   )
# 
# ggsave("Figures/infant/hmis_counts.png", width = 6, height = 5)
# 
# 
# # normalized to compare the trends
# db_all3 %>% 
#   filter(indic %in% c("neonatal_deaths", 
#                       "stillbirths",
#                       "deliveries")) %>% 
#   mutate(indic = factor(indic, levels = c("deliveries",
#                                           "stillbirths", 
#                                           "neonatal_deaths"))) %>% 
#   ggplot()+
#   geom_line(aes(Date, std_val))+
#   facet_grid(indic ~ Country, scale = "free")+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   # labs(title = indr)+
#   theme_bw()+
#   theme(
#     legend.position = "bottom"
#   )
# 
# ggsave("Figures/infant/hmis_counts_normalized.png", width = 6, height = 5)

db_all3 %>% 
  filter(indic %in% c("neonatal_rates", 
                      "fetal_rates", 
                      "peri_neonatal_rates")) %>% 
  mutate(indic = factor(indic, levels = c("fetal_rates", 
                                          "peri_neonatal_rates",
                                          "neonatal_rates"))) %>% 
  ggplot()+
  geom_line(aes(Date, Value))+
  scale_x_date(date_labels = "%yy")+
  facet_grid(indic ~ Country, scale = "free")+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

ggsave("Figures/infant/hmis_rates.png", width = 6, height = 5)

db_all3 %>% 
  filter(indic %in% c("neonatal_rates", 
                      "fetal_rates", 
                      "peri_neonatal_rates")) %>% 
  mutate(indic = factor(indic, levels = c("fetal_rates", 
                                          "peri_neonatal_rates",
                                          "neonatal_rates"))) %>% 
  ggplot()+
  geom_line(aes(Date, std_val))+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "blue", alpha = 0.4, size = 1)+
  scale_x_date(date_labels = "%yy")+
  facet_grid(indic ~ Country, scale = "free")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  # labs(title = indr)+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

ggsave("Figures/infant/hmis_normalized_rates.png", width = 6, height = 5)



# ~~~~~~~~~~~~~~~~~ 
# cumulative values
# ~~~~~~~~~~~~~~~~~

# in quarters
db_quarter <- 
  db_all2 %>% 
  mutate(qtr = paste(Year, Quarter, sep = "_")) %>% 
  group_by(Country, indic, qtr) %>% 
  summarise(Value = sum(Value),
            Date = mean(Date)) %>% 
  ungroup()

# annual
db_annual <- 
  db_all2 %>% 
  group_by(Country, indic, Year) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Date = Year)


# Plots
# ~~~~~

# Quarters
plot_this(db_quarter, "child_deaths")
plot_this(db_quarter, "neonatal_deaths")
plot_this(db_quarter, "stillbirths")
plot_this(db_quarter, "deliveries")
plot_this(db_quarter, "d_maternal")

plot_this(db_quarter, "neonatal_rates")
plot_this(db_quarter, "fetal_rates")
plot_this(db_quarter, "peri_neonatal_rates")

db_quarter2 <- 
  db_quarter %>% 
  group_by(Country, indic) %>% 
  mutate(av_ind = mean(Value),
         norm_ind = Value/av_ind)

db_quarter2 %>% 
  filter(indic %in% c("neonatal_rates", 
                      "fetal_rates", 
                      "peri_neonatal_rates")) %>% 
  mutate(indic = factor(indic, levels = c("fetal_rates", 
                                          "peri_neonatal_rates",
                                          "neonatal_rates"))) %>% 
  ggplot()+
  geom_line(aes(Date, norm_ind))+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "blue", alpha = 0.4, size = 1)+
  scale_x_date(date_labels = "%yy")+
  facet_grid(indic ~ Country)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

ggsave("Figures/infant/hmis_normalized_rates_quarter.png", width = 6, height = 5)




# Annual
plot_this(db_annual, "child_deaths")
plot_this(db_annual, "neonatal_deaths")
plot_this(db_annual, "stillbirths")
plot_this(db_annual, "deliveries")
plot_this(db_annual, "d_maternal")

plot_this(db_annual, "neonatal_rates")
plot_this(db_annual, "fetal_rates")
plot_this(db_annual, "peri_neonatal_rates")

db_annual %>% 
  filter(indic %in% c("neonatal_rates", "fetal_rates", "peri_neonatal_rates")) %>% 
  ggplot()+
  geom_line(aes(Date, Value))+
  scale_x_continuous(breaks = 2018:2020)+
  facet_grid(indic ~ Country, scales = "free_y")+
  theme_bw()

