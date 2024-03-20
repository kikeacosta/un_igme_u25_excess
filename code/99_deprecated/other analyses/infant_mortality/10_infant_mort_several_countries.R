library(here)
source(here("Code", "00_functions.R"))

# brazil
bra <- 
  read_rds("Output/infant/brazil_monthly_inf_deaths_births_2015_2021.rds") %>% 
  mutate(country = "brazil")
# chile
chl <- 
  read_rds("Output/infant/chile_monthly_inf_deaths_births_2016_2021.rds") %>% 
  mutate(country = "chile")
# colombia
col <- 
  read_rds("Output/infant/colombia_annual_inf_deaths_births_2015_2020.rds") %>%
  mutate(country = "colombia")
# ecuador
ecu <- 
  read_rds("Output/infant/ecuador_annual_inf_deaths_births_2008_2020.rds") %>% 
  mutate(country = "ecuador")
# costa rica
cri <- 
  read_rds("Output/infant/costa_rica_annual_inf_deaths_births_2015_2020.rds") %>% 
  mutate(country = "costa rica")
# peru (only deaths, no births)
per <- 
  read_rds("Output/infant/peru_monthly_inf_deaths_2017_2021.rds")  %>% 
  mutate(country = "peru")

# TODO: uruguay ()
uru <- read_rds("Output/infant/ecuador_annual_inf_deaths_births_2008_2020.rds")


# neonatal
# monthly

db_mth <- 
  bind_rows(bra, chl)


# annual
db_ann <- 
  db_mth %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year, age) %>% 
  summarise(deaths = sum(deaths), 
            births = sum(births)) %>% 
  ungroup() %>% 
  mutate(mx = deaths / births) %>% 
  bind_rows(col, ecu, cri) %>% 
  filter(year >= 2015 & year <= 2020)

unique(db_ann$country)


db_ann %>% 
  ggplot()+
  geom_line(aes(year, mx))+
  facet_grid(age ~ country, scales = "free_y")+
  theme_bw()

db_ch <- 
  db_ann %>% 
  arrange(country, age, year) %>% 
  group_by(country, age) %>% 
  mutate(change = mx / lag(mx),
         y2 = str_sub(year, 3, 4) %>% as.double() + 0.5)


db_ch %>% 
  ggplot()+
  geom_line(aes(y2, change), alpha = 0.8)+
  geom_point(aes(y2, change), size = 0.5)+
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5)+  
  geom_vline(xintercept = 20, col = "red", alpha = 0.5)+  
  
  facet_grid(age ~ country, scales = "free_y")+
  labs(x = "year", y = "annual rate change")+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10) 
  )

ggsave("Figures/infant/change_infant_mortality_several_annual.png", 
       dpi = 600,
       width = 6,
       height = 3)


