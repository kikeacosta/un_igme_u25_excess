library(here)
source(here("Code", "00_functions.R"))
# Mexico mortality data
# ~~~~~~~~~~~~~~~~~~~~~
# # files from OSF (Version 1) as of 15 March 2021 
# osf_retrieve_file("hbxkn") %>%
#   osf_download(conflicts = "overwrite",
#                path = "Data")
# 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

db_mex_0 <-
  out %>% 
  bind_rows() %>% 
  mutate(time_unit = str_sub(edad, 1, 1),
         time_unit = recode(time_unit,
                            !!!time_units),
         age_val = str_sub(edad, 2, 4) %>% as.integer(),
         Age = case_when(time_unit %in% c("mins", "hrs") |
                           (time_unit %in% c("days") & age_val <= 7) ~ "Perinatal (0-7)",
                         time_unit %in% c("days") & age_val >= 8 ~ "Neonatal (8-30)",
                         time_unit %in% c("mts") ~ "Postneonatal (30-364)")) %>% 
  filter(time_unit %in% c("mins", "hrs", "days", "mts")) %>% 
  mutate(diff = Date_reg - Date,
         diff = ifelse(diff < 0, 0, diff),
         Age = factor(Age, levels = c("Perinatal (0-7)", 
                                      "Neonatal (8-30)", 
                                      "Postneonatal (30-364)")),
         Source = Source %>% as.integer()) %>% 
  filter(Source >= 2015)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# all deaths from years 2020 and 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_mx20 <- 
  read_csv(here("Data", "Mexico", "DDAAxsom2021SE14.csv"))

db_mx20_0 <- 
  db_mx20 %>% 
  filter(EDAD == 0) %>% 
  select(Age = EDAD, Date = 'FECHA_DEFUNCION', Date_reg = FECHA_REGISTRO)

prop_reg_2020 <- 
  db_mx20_0 %>% 
  filter(Date <= "2020-12-31") %>% 
  mutate(Year_reg = year(Date_reg)) %>% 
  group_by(Year_reg) %>% 
  summarise(Deaths = n()) %>% 
  group_by() %>% 
  mutate(prop = Deaths / sum(Deaths))

db_mx20_0_2 <- 
  db_mx20_0 %>% 
  mutate(Date = make_date(d = 15, m = month(Date), y = year(Date))) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup()

db_0_class <- 
  db_mex_0  %>% 
  mutate(Date = make_date(d = 15, m = month(Date), y = year(Date))) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  bind_rows(db_mx20_0_2) %>% 
  filter(Date >= "2015-01-01" & Date <= "2020-12-31")

db_0_class %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))+
  theme_bw()+
  labs(title = "Mexico: infant mortality by month")
ggsave("Figures/infant/infant_mortality_by_month_mexico.png")

# annual mortality
db_0_year <- 
  db_0_class %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_0_year %>% 
  ggplot()+
  geom_line(aes(Year, Deaths))+
  theme_bw()+
  labs(title = "Mexico: infant mortality by year")
ggsave("Figures/infant/infant_mortality_by_year_mexico.png")

# mortality change
db_0_year_ch <- 
  db_0_year %>% 
  mutate(ch = Deaths / lag(Deaths) - 1)

db_0_year_ch %>% 
  ggplot()+
  geom_line(aes(Year, ch))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_continuous(breaks = seq(2016, 2020, 1), limits = c(2016, 2020))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme_bw()+
  labs(title = "Mexico: infant mortality change")
ggsave("Figures/infant/infant_mortality_change_mexico.png")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis of time elapsed between death and registration
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

year_reg <- 
  db_mex_0  %>%  
  mutate(Year_reg = year(Date_reg) - year(Date),
         Year_reg = case_when(Year_reg == 0 ~ "Same year",
                              TRUE ~ paste0("Year + ", Year_reg))) %>% 
  group_by(Age, Source, Year_reg) %>% 
  summarise(Deaths = n()) %>% 
  filter(Deaths > 5) %>% 
  group_by(Age, Source) %>% 
  mutate(prop = Deaths / sum(Deaths)) %>% 
  ungroup()

year_reg %>% 
  group_by(Year_reg) %>% 
  summarise(prop_mean = mean(prop),
            min_prop = min(prop),
            max_prop = max(prop))

year_reg %>% 
  # filter(Source <= 2018) %>% 
  ggplot() +
  geom_bar(aes(x = Deaths, y = Age, fill = factor(Year_reg)), 
           position="fill", stat="identity")+
  labs(fill = "Year", title = "Infant mortality registration delay",
       x = "Prop. Deaths")+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  facet_grid(~Source)+
  theme_bw()
# ggsave("Figures/infant/mexico_registration_year_deaths.png",
#        width = 8, height = 4)


time_reg <- 
  db_mex_0 %>% 
  select(Source, Age, diff) %>% 
  mutate(diff = diff %>% as.integer()) %>% 
  filter(diff < 1000)

avs <- 
  time_reg %>% 
  group_by(Source, Age) %>% 
  summarise(av = mean(diff)) %>% 
  ungroup()

time_reg %>%
  ggplot(aes(x = diff, fill = Age)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  geom_vline(data=avs, aes(xintercept=av,  colour=Age),
             linetype="dashed", size=1)+
  geom_boxplot(width = 100, alpha = 0.3)+
  facet_grid(~Source)

time_reg %>%
  # filter(Source <= 2018) %>% 
  ggplot(aes(x = diff, col = Age)) +
  geom_density()+
  geom_vline(data = avs, aes(xintercept = av, colour = Age),
             linetype = "dashed", size = 0.5)+
  geom_boxplot(width = 0.001, alpha = 0.3)+
  labs(x = "days", title = "Register delay")+
  theme_bw()+
  facet_grid(~Source)+
  theme(
    legend.position = "bottom"
  )
ggsave("Figures/infant/mexico_registration_delay_days_by_year.png",
       width = 8, height = 5)



