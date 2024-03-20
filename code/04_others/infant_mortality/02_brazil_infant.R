library(here)
source(here("Code", "00_functions.R"))
# Source of data
# https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-mortalidade-sim-1979-a-2019
# dictionnary
# Source: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Estrutura_SIM.pdf

# Variable (Age) IDADE:
# ~~~~~~~~~~~~~~~~~~~
# Idade do falecido em minutos, horas, dias, meses ou anos. (Idade:
# composto de dois subcampos. - O primeiro, de 1 dígito, indica a
# unidade da idade (se 1 = minuto, se 2 = hora, se 3 = mês, se 4 = ano,
# se = 5 idade maior que 100 anos). - O segundo, de dois dígitos, indica a
# quantidade de unidades: 
# Idade menor de 1 hora: subcampo varia de 01 e 59 (minutos); 
# De 1 a 23 Horas: subcampo varia de 01 a 23 (horas);
# De 24 horas e 29 dias: subcampo varia de 01 a 29 (dias); 
# De 1 a menos de 12 meses completos: subcampo varia de 01 a 11 (meses); Anos -
# subcampo varia de 00 a 99; - 9 - ignorado)

# Variable: LINHAA (ICD Code)
# ~~~~~~~~~~~~~~~~
# CIDs informados na Linha A da DO referente ao diagnóstico na Linha A
# da DO (causa terminal - doença ou estado mórbido que causou
# diretamente a morte). (Códigos CID 10)

# Date variables
# ~~~~~~~~~~~~~~
# DTOBITO: Data em que occoreu o óbito.(Data no padrão ddmmaaaa) 
# DTRECEBIM: Data do recebimento. (Data no padrão ddmmaaaa)
# DTRECORIG: Data do recebimento original. (Data no padrão ddmmaaaa)


# Note: I assume that 2 corresponds to days. It is sais that a count 
# for days exists, and there i sno indication for value 0 in the indicator 
# of time 
time_units <- 
  c("0" = "mins",
    "1" = "hrs", 
    "2" = "days", 
    "3" = "mts", 
    "4" = "yrs",
    "5" = ">100")

links <- 
  c(paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_",2015:2019,".csv"),
    "https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31/download/dobrano_.csv")

# links <- unzip(here("Data", "Brazil", "brazil_deaths.zip"), list = TRUE)
# 
# test <- 
#   read_csv2(unzip(here("Data", "Brazil", "brazil_deaths.zip"), links[1,1]))

links <- tibble(Year = 2015:2020,
                File = paste0(here("Data", "Brazil"), "/Mortalidade_Geral_", 2015:2020, ".csv"))

out <- list()
for (i in 1:nrow(links)){
  
  cat(links[i,1] %>% as.character(),"\n")
  out[[i]] <- read_delim(links[i,2] %>% as.character(), delim = ";") %>%  
    select(age_val = IDADE, Cause = LINHAA, Date = DTOBITO, 
           Date_reg = DTRECEBIM, diff = DIFDATA) %>% 
    mutate(Date = dmy(Date),
           Date_reg = dmy(Date_reg),
           Date_reg2 = Date + days(diff),
           time_unit = str_sub(age_val, 1, 1),
           time_unit = recode(time_unit,
                              !!!time_units),
           age_val = str_sub(age_val, 2, 3) %>% as.integer()) %>% 
    filter(time_unit %in% c("mins", "hrs", "days", "mts")) %>% 
    mutate(Age = case_when(time_unit %in% c("mins", "hrs") |
                             (time_unit %in% c("days") & age_val <= 7) ~ "Perinatal (0-7)",
                           time_unit %in% c("days") & age_val >= 8 ~ "Neonatal (8-30)",
                           time_unit %in% c("mts") ~ "Postneonatal (30-364)"),
           Source = links[i,1] %>% as.character())
  
}

db_bra_0 <-
  out %>% 
  bind_rows() %>% 
  mutate(Age = factor(Age, levels = c("Perinatal (0-7)", 
                                      "Neonatal (8-30)", 
                                      "Postneonatal (30-364)")))

db_0_class <- 
  db_bra_0  %>% 
  mutate(Date = make_date(d = 15, m = month(Date), y = year(Date))) %>% 
  group_by(Age, Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup()

db_0_class %>% 
  ggplot()+
  geom_area(aes(Date, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Brazil: infant mortality by month")
ggsave("Figures/infant/infant_mortality_by_month_brazil.png")


db_0_class %>% 
  ggplot()+
  geom_line(aes(Date, Deaths, col = Age))

db_causes <- 
  db_bra_0 %>% 
  group_by()


# annual mortality
db_0_year <- 
  db_0_class %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Age, Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_0_year %>% 
  ggplot()+
  geom_area(aes(Year, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Brazil: infant mortality by year")
ggsave("Figures/infant/infant_mortality_by_year_brazil.png")

# mortality change
db_0_year_ch <- 
  db_0_year %>% 
  group_by(Age) %>% 
  mutate(ch = Deaths / lag(Deaths) - 1)

db_0_year_ch %>% 
  ggplot()+
  geom_line(aes(Year, ch, col = Age))+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(2016, 2020, 1), limits = c(2016, 2020))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme_bw()+
  labs(title = "Brazil: infant mortality change")
ggsave("Figures/infant/infant_mortality_change_brazil.png")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Looking at registration delay
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_bra_0 <-
  out %>% 
  bind_rows()%>% 
  mutate(Age = factor(Age, levels = c("Perinatal (0-7)", 
                                      "Neonatal (8-30)", 
                                      "Postneonatal (30-364)")))

year_reg <- 
  db_bra_0  %>%  
  mutate(Year_reg = year(Date_reg2) - year(Date),
         Year_reg = recode(Year_reg,
                           "0" = "Same year",
                           "1" = "Year + 1",
                           "2" = "Year + 2",
                           "3" = "Year + 3")) %>% 
  group_by(Age, Source, Year_reg) %>% 
  summarise(Deaths = n()) %>% 
  group_by(Age, Source) %>% 
  mutate(prop = Deaths / sum(Deaths))

year_reg %>% 
  filter(Source <= 2018) %>% 
  group_by(Year_reg) %>% 
  summarise(prop = mean(prop))


cols <- c("#00b4d8", "#0077b6", "#03045e")

year_reg %>% 
  filter(Source <= 2018) %>% 
  ggplot() +
  geom_bar(aes(x = Deaths, y = Age, fill = factor(Year_reg)), 
           position="fill", stat="identity")+
  scale_fill_manual(values = cols)+
  labs(fill = "Year", title = "Infant mortality registration delay",
       x = "Prop. Deaths")+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  facet_grid(~Source)+
  theme_bw()
ggsave("Figures/infant/brazil_registration_year_deaths.png",
       width = 8, height = 4)

time_reg <- 
  db_bra_0 %>% 
  select(Source, Age, diff) %>% 
  mutate(diff = diff %>% as.integer()) 
  
avs <- 
  time_reg %>% 
  group_by(Source, Age) %>% 
  summarise(av = mean(diff),
            md = median(diff)) %>% 
  ungroup()

time_reg %>%
  ggplot(aes(x = diff, fill = Age)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  geom_vline(data=avs, aes(xintercept=av,  colour=Age),
             linetype="dashed", size=1)+
  geom_boxplot(width = 100, alpha = 0.3)+
  facet_grid(~Source)

time_reg %>%
  filter(Source <= 2018) %>% 
  ggplot(aes(x = diff, col = Age)) +
  geom_density()+
  geom_vline(data = avs %>% filter(Source <= 2018), aes(xintercept = av, colour = Age),
             linetype = "dashed", size = 0.5)+
  geom_boxplot(width = 0.001, alpha = 0.3)+
  labs(x = "days", title = "Register delay")+
  theme_bw()+
  facet_grid(~Source)+
  theme(
    legend.position = "bottom"
  )
ggsave("Figures/infant/brazil_registration_delay_days_by_year.png",
       width = 8, height = 5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

year_reg %>% 
  filter(Source == 2020) %>% 
  group_by(Year_reg) %>% 
  summarise(prop = mean(prop))

regist <- 
  db_bra_0  %>%  
  mutate(Year_reg = year(Date_reg2) - year(Date),
         Year_reg = recode(Year_reg,
                           "0" = "Same year",
                           "1" = "Year + 1",
                           "2" = "Year + 2",
                           "3" = "Year + 3"),
         fast = case_when(Year_reg == "Same year" | (Year_reg == "Year + 1" & Date_reg2 <= make_date(d = 2, m = 3, y = as.integer(Source) + 1)) ~ 1,
                          TRUE ~ 0))

regist %>% 
  group_by(Source, fast) %>% 
  summarise(Deaths = n()) %>% 
  group_by(Source) %>% 
  mutate(prop = Deaths / sum(Deaths))

db_bra_0_comp <- 
  db_bra_0  %>%  
  mutate(Year_reg = year(Date_reg2) - year(Date),
         Year_reg = recode(Year_reg,
                           "0" = "Same year",
                           "1" = "Year + 1",
                           "2" = "Year + 2",
                           "3" = "Year + 3"),
         fast = case_when(Year_reg == "Same year" | (Year_reg == "Year + 1" & Date_reg2 <= make_date(d = 2, m = 3, y = as.integer(Source) + 1)) ~ 1,
                          TRUE ~ 0)) %>% 
  filter(fast == 1)



db_0_class_comp <- 
  db_bra_0_comp  %>% 
  mutate(Date = make_date(d = 15, m = month(Date), y = year(Date))) %>% 
  group_by(Age, Date) %>% 
  summarise(Deaths = n()) %>% 
  ungroup()

db_0_class_comp %>% 
  ggplot()+
  geom_area(aes(Date, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Brazil: infant mortality by month")
ggsave("Figures/infant/infant_mortality_by_month_brazil_adj.png")


db_0_class_comp %>% 
  ggplot()+
  geom_line(aes(Date, Deaths, col = Age))


# annual mortality
db_0_year_comp <- 
  db_0_class_comp %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Age, Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_0_year_comp %>% 
  ggplot()+
  geom_area(aes(Year, Deaths, fill = Age))+
  theme_bw()+
  labs(title = "Brazil: infant mortality by year")
ggsave("Figures/infant/infant_mortality_by_year_brazil_adj.png")

# mortality change
db_0_year_ch <- 
  db_0_year_comp %>% 
  group_by(Age) %>% 
  mutate(ch = Deaths / lag(Deaths) - 1)

db_0_year_ch %>% 
  ggplot()+
  geom_line(aes(Year, ch, col = Age))+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(2016, 2020, 1), limits = c(2016, 2020))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme_bw()+
  labs(title = "Brazil: infant mortality change")
ggsave("Figures/infant/infant_mortality_change_brazil_adj.png")

births <- tibble(Year = 2015:2019,
                 Births = c(2952969, 
                            2803080,
                            2874466,
                            2899851,
                            2812030))

births %>% 
  ggplot()+
  geom_line(aes(Year, Births))+
  theme_bw()
