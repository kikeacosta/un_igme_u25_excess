library(here)
source(here("Code", "00_functions.R"))
library(readxl)

# ~~~~~~~~~~~~~~~~~~~
# Peru mortality data
# ~~~~~~~~~~~~~~~~~~~
# files from OSF (Version 1) as of 15 March 2021 

# load data
db_pe <- 
  read_xlsx(unzip(here("Data", "Peru", "peru_deaths.zip"),
                  "SINADEF_DATOS_ABIERTOS_19032021.xlsx"), skip = 2)

# db_pe <- 
#   read_xlsx(here("SINADEF_DATOS_ABIERTOS_19032021.xlsx"), 
#             skip = 2)


# less than 1-year old
# ~~~~~~~~~~~~~~~~~~~~
db_pe_0 <- 
  db_pe %>% 
  rename(time_unit = `TIEMPO EDAD`) %>% 
  mutate(Age = as.integer(EDAD)) %>% 
  filter(!time_unit %in% c("AÑOS", "IGNORADO", "SIN REGISTRO"))

unique(db_pe_0$time_unit)

db_pe0_2 <- 
  db_pe_0 %>% 
  mutate(Date = make_date(y = AÑO, m = MES, d = 15)) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(t = 1:n())

# plot of all deaths at age 0
db_pe0_2 %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))


# Perinatal mortality (less than a 7 days)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_pe_per <- 
  db_pe_0 %>% 
  filter(time_unit %in% c("HORAS", "MINUTOS", "SEGUNDOS") |
           time_unit == "DIAS" &  Age <= 7) %>% 
  mutate(Date = make_date(y = AÑO, m = MES, d = 15)) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Age = "Perinatal (0-7)")

db_pe_per %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))

# Neonatal mortality (8 days to 1 month)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_pe_neo <- 
  db_pe_0 %>% 
  filter(time_unit == "DIAS" & Age >= 8) %>% 
  mutate(Date = make_date(y = AÑO, m = MES, d = 15)) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Age = "Neonatal (8-30)")

db_pe_neo %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))


# Postneonatal mortality (29-days to 1 year)
db_pe_pos <- 
  db_pe_0 %>% 
  filter(time_unit %in% c("MESES")) %>% 
  mutate(Date = make_date(y = AÑO, m = MES, d = 15)) %>% 
  group_by(Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Age = "Postneonatal (30-364)")

db_pe_pos %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))


# merging the three datasets together
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_0_class <- 
  bind_rows(db_pe_neo,
            db_pe_per,
            db_pe_pos) %>% 
  mutate(Age = factor(Age, levels = c("Perinatal (0-7)", 
                                      "Neonatal (8-30)", 
                                      "Postneonatal (30-364)")))

db_0_class %>% 
  ggplot()+
  geom_area(aes(Date, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Peru: infant mortality by month")
ggsave("Figures/infant/infant_mortality_by_month_peru.png")


# annual mortality
db_0_year <- 
  db_0_class %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Age, Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_0_year %>% 
  filter(Year <= 2020) %>% 
  ggplot()+
  geom_area(aes(Year, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Peru: infant mortality by year")
ggsave("Figures/infant/infant_mortality_by_year_peru.png")

# mortality change
db_0_year_ch <- 
  db_0_year %>% 
  group_by(Age) %>% 
  mutate(ch = Deaths / lag(Deaths) - 1)

db_0_year_ch %>% 
  filter(Year <= 2020) %>% 
  ggplot()+
  geom_line(aes(Year, ch, col = Age))+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(2016, 2020, 1), limits = c(2018, 2020))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme_bw()+
  labs(title = "Peru: infant mortality change")
ggsave("Figures/infant/infant_mortality_change_peru.png")



# Looking at causes of death
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# on progress, it seems quite complex due to lack of standardization
db_cause_0 <- 
  db_pe_0 %>%
  mutate(Age = case_when(time_unit %in% c("MESES") ~ "Postneonatal (30-364)",
                         time_unit %in% c("DIAS") & Age >= 8 ~ "Neonatal (8-30)",
                         time_unit %in% c("HORAS", "MINUTOS", "SEGUNDOS") |
                           (time_unit == "DIAS" &  Age <= 7) ~ "Perinatal (0-7)"))
  
db_cause_0_2 <- 
  db_cause_0 %>% 
  rename(cause_a = 'DEBIDO A (CAUSA A)',
         code_a = 'CAUSA A (CIE-X)') %>% 
  group_by(Age, cause_a, code_a) %>% 
  summarise(Deaths = n())
  
causes <- 
  db_cause_0_2 %>% 
  group_by(cause_a) %>% 
  summarise(Deaths = sum(Deaths))
  
codes <- 
  db_cause_0_2 %>% 
  group_by(code_a) %>% 
  summarise(Deaths = sum(Deaths))

causes_codes <- 
  db_cause_0_2 %>% 
  group_by(code_a, cause_a) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  group_by(code_a) %>% 
  mutate(order = 1:n())
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





db_pe_1_4 <- 
  db_pe %>% 
  rename(time_unit = `TIEMPO EDAD`,
         Age = EDAD) %>% 
  mutate(Age = as.integer(Age)) %>% 
  filter(time_unit %in% c("AÑOS"),
         Age < 5)
  

unique(db_pe_1_4$Age)


# data wrangling
db_pe2 <- db_pe %>% 
  select(Sex = SEXO,
         Age = EDAD, 
         Year = AÑO,
         unit_age = 'TIEMPO EDAD') %>% 
  mutate(Sex = recode(Sex,
                      "MASCULINO" = "m",
                      "FEMENINO" = "f"))