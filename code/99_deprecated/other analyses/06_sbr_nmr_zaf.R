library(here)
source(here("Code", "00_functions.R"))

# Stillbirth and neonatal rates in South Africa
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_zaf <- read_csv(here("Data", "south_africa_stillbirth_neonatal.csv")) %>% 
  gather(-year, key = measure, value = rate)

db_zaf %>% 
  ggplot()+
  geom_point(aes(year, rate))+
  theme_bw()+
  facet_wrap(~measure, scales = "free")

ggsave("Figures/ZAF/ZAF_stillbirth_neonatal_rates.png", 
       dpi = 600)

