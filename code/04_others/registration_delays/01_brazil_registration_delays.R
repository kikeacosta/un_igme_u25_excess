library(here)
source(here("Code", "00_functions.R"))

db_bra <- read_rds("Output/brazil_micro_2015_2020.rds")

year_reg <- 
  db_bra %>% 
  mutate(Age = case_when(time_unit != "yrs" ~ 0,
                         time_unit == "yrs" & age_val >= 1 & age_val <= 4 ~ 1,
                         time_unit == "yrs" & age_val >= 5 & age_val <= 9 ~ 5,
                         time_unit == "yrs" & age_val >= 10 & age_val <= 14 ~ 10,
                         time_unit == "yrs" & age_val >= 15 & age_val <= 24 ~ 15,
                         TRUE ~ 99)) %>% 
  filter(Age < 99) %>% 
  mutate(Year = year(Date),
         Year_reg = year(Date_reg) - year(Date),
         Year_reg = case_when(Year_reg == 0 ~ "Same year",
                              Year_reg == 1 ~ "Year + 1",
                              Year_reg >= 2 ~ "Year + 2")) %>% 
  filter(Year >= 2015) %>% 
  drop_na(Year_reg) %>% 
  group_by(Age, Year, Year_reg) %>% 
  summarise(Deaths = n()) %>% 
  group_by(Age, Year) %>% 
  mutate(prop = Deaths / sum(Deaths)) %>% 
  ungroup() 

cols <- c("#00b4d8", "#0077b6", "#03045e")

year_reg %>% 
  filter(Year <= 2018) %>%
  ggplot() +
  geom_bar(aes(x = prop, y = as_factor(Age), fill = factor(Year_reg)), 
           position="fill", stat="identity")+
  scale_fill_manual(values = cols)+
  labs(fill = "Year", title = "Deaths registration delay",
       x = "Prop. Deaths")+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  facet_grid(~ Year)+
  theme_bw()

ggsave("Figures/registration_delay/brazil_deaths_registration_delay.png")

year_reg %>% 
  ungroup() %>% 
  group_by(Year_reg) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  group_by()%>% 
  summarise(prop = Deaths / sum(Deaths)) 

year_reg %>% 
  ungroup() %>% 
  group_by(Year, Year_reg) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  group_by(Year)%>% 
  summarise(prop = Deaths / sum(Deaths)) 


