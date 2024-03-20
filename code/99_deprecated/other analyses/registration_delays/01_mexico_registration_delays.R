library(here)
source(here("Code", "00_functions.R"))


# Mexico mortality data
# ~~~~~~~~~~~~~~~~~~~~~
# all deaths between 2012 and 2020

mex_files <- unzip(here("Data", "Mexico", "mexico_deaths.zip"), list = TRUE)
# 
out <- list()
for (i in 1:nrow(mex_files)){
  
  year_loc <- mex_files[i,1] %>% str_locate(c(".CSV", ".csv")) 
  year <- ifelse(!is.na(year_loc[1,1]), str_sub(mex_files[i,1], year_loc[1,1] - 4, year_loc[1,1] - 1),
                 str_sub(mex_files[i,1], year_loc[2,1] - 4, year_loc[2,1] - 1))
  
  out[[i]] <- 
    read_csv(unz(here("Data", "Mexico", "mexico_deaths.zip"), mex_files[i,1])) %>% 
    mutate(Source = year,
           dia_regis = ifelse(dia_regis == 99, 15, dia_regis),
           dia_ocurr = ifelse(dia_ocurr == 99, 15, dia_ocurr),
           mes_ocurr = ifelse(mes_ocurr == 99, 6, mes_ocurr),
           Date = make_date(d = dia_ocurr, m = mes_ocurr, y = anio_ocur),
           Date_reg = make_date(d = dia_regis, m = mes_regis, y = anio_regis)) %>% 
    select(edad, Date, Date_reg, Source) 
  
}

# ages
# 1 minutes/hours
# 2 days
# 3 months
# 4 years
time_units <- 
  c("1" = "hrs", 
    "2" = "days", 
    "3" = "mts", 
    "4" = "yrs")

db_mex <-
  out %>% 
  bind_rows() %>% 
  mutate(time_unit = str_sub(edad, 1, 1),
         time_unit = recode(time_unit,
                            !!!time_units),
         age_val = str_sub(edad, 2, 4) %>% as.double(),
         Age = case_when(time_unit %in% c("mins", "hrs", "days", "mts") ~ 0,
                         time_unit %in% c("yrs") ~ age_val)) %>% 
  mutate(diff = Date_reg - Date,
         diff = ifelse(diff < 0, 0, diff),
         Source = Source %>% as.integer()) %>% 
  filter(Source >= 2015)

unique(db_mex$time_unit)
unique(db_mex$Age) %>% sort()


db_mex2 <-
  db_mex %>% 
  filter(Age < 25) %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age >= 1 & Age <= 4 ~ 1,
                         Age >= 5 & Age <= 9 ~ 5,
                         Age >= 10 & Age <= 14 ~ 10,
                         Age >= 15 & Age <= 24 ~ 15)) %>% 
  group_by(Age) %>% 
  mutate(av_delay = mean(diff)) %>% 
  ungroup()


year_reg <- 
  db_mex2  %>%  
  mutate(Year_reg = year(Date_reg) - year(Date),
         Year_reg = case_when(Year_reg == 0 ~ "Same year",
                               Year_reg == 1 ~ "Year + 1",
                               Year_reg >= 2 ~ "Year + 2"),
         Year = year(Date)) %>% 
  drop_na(Year_reg) %>% 
  filter(Year >= 2015) %>% 
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

ggsave("Figures/registration_delay/mexico_deaths_registration_delay.png")

year_reg %>% 
  group_by(Year_reg) %>% 
  summarise(prop = mean(prop))

