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

# source of micro_data
# links <- 
#   c(paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_",2015:2019,".csv"),
#     "https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/8d947ac1-addb-49f2-85ab-824a7408a432/download/dobrano_.csv",
#     "https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31/download/do2021opendata.csv")

links <- tibble(Year = 2015:2021,
                File = paste0(here("Data", "Brazil", "deaths"), "/Mortalidade_Geral_", 2015:2021, ".csv"))

out <- list()
# for (i in 1:nrow(links)){
for (i in 6:7){
    
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
                             (time_unit %in% c("days") & age_val <= 7) ~ "neo_ear",
                           time_unit %in% c("days") & age_val >= 8 ~ "neo_lat",
                           time_unit %in% c("mts") ~ "post_neo"),
           Source = links[i,1] %>% as.character())
  
}

db_inf <-
  out %>% 
  bind_rows() %>% 
  mutate(age = factor(Age, levels = c("neo_ear", 
                                      "neo_lat", 
                                      "post_neo")))

db_inf_mth <- 
  db_inf  %>% 
  mutate(date = make_date(d = 15, m = month(Date), y = year(Date))) %>% 
  group_by(age, date) %>% 
  summarise(deaths = n()) %>% 
  ungroup()

# live births
bs20 <- read_csv2("Data/Brazil/births_monthly_2020.csv")
bs21 <- read_csv2("Data/Brazil/births_monthly_2021.csv")
bs1519 <- read_csv2("Data/Brazil/births_monthly_2015_2019.csv", skip = 3)

# preparing births data
# ~~~~~~~~~~~~~~~~~~~~~
bs1519_2 <- 
  bs1519 %>% 
  drop_na() %>% 
  mutate(month = 1:n()) %>% 
  filter(month != 13) %>% 
  select(-1, -Total) %>%
  gather(-month, key = year, value = births) %>% 
  mutate(date = make_date(d = 15, m = month, y = year)) %>% 
  select(date, births)

bs20_2 <- 
  bs20 %>% 
  select(loc = 50, 51:63) %>% 
  filter(loc == "Brasil") %>% 
  gather(-loc, key = month_l, value = births) %>% 
  mutate(month = 1:n()) %>% 
  filter(month != 13) %>% 
  mutate(date = make_date(d = 15, m = month, y = 2020)) %>% 
  select(date, births)

bs21_2 <- 
  bs21 %>% 
  select(loc = 50, 51:63) %>% 
  filter(loc == "Brasil") %>% 
  gather(-loc, key = month_l, value = births) %>% 
  mutate(month = 1:n()) %>% 
  filter(month <= 5) %>% 
  mutate(date = make_date(d = 15, m = month, y = 2021)) %>% 
  select(date, births)

bs_all <- 
  bind_rows(bs1519_2, bs20_2, bs21_2)


# merging monthly deaths and births
db <- 
  db_inf_mth %>% 
  left_join(bs_all) %>% 
  mutate(mx = deaths / births)

db %>% 
  filter(age %in% c("neo_ear", "neo_lat")) %>% 
  filter(date <= "2021-03-15") %>% 
  ggplot()+
  geom_line(aes(date, mx))+
  geom_point(aes(date, mx), size = 0.3)+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  facet_wrap(~ age, scales = "free", ncol = 1)+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )

ggsave("Figures/infant/neonatal_mortality_brazil_monthly.png", 
       dpi = 600,
       width = 6,
       height = 3)

write_rds(db, "Output/infant/brazil_monthly_inf_deaths_births_2015_2021.rds")


