rm (list = ls()); gc()
source("Code/00_functions.R")
# Source of data
# https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-mortalidade-sim-1979-a-2019
# dictionnary
# Source: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Estrutura_SIM.pdf
# https://svs.aids.gov.br/daent/centrais-de-conteudos/dados-abertos/sim/


# # data in dbf format
# # ~~~~~~~~~~~~~~~~~~
# library(foreign)
# d21 <- read.dbf("data_input/Brazil/deaths/DO21OPEN.dbf")
# d22 <- read.dbf("data_input/Brazil/deaths/DO22OPEN.dbf")
# write_csv(d21, "data_input/Brazil/deaths/Mortalidade_Geral_2021.csv")
# write_csv(d22, "data_input/Brazil/deaths/Mortalidade_Geral_2022.csv")

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

# data loading
# ============
time_units <- 
  c("0" = "mins",
    "1" = "hrs", 
    "2" = "days", 
    "3" = "mts", 
    "4" = "yrs",
    "5" = ">100",
    "9" = "unk")

links <- paste0("data_input/Brazil/deaths/Mortalidade_Geral_", 2015:2022, ".csv")
i <- links[1]
out <- list()
for (i in links){
  
  cat(i)
  
  separ <- ifelse(i %in% c("data_input/Brazil/deaths/Mortalidade_Geral_2021.csv", 
                           "data_input/Brazil/deaths/Mortalidade_Geral_2022.csv"),
                  ",", ";")
  
  out[[i]] <- read_delim(i, 
                         delim = separ,
                         col_types = cols(.default = "c")) %>%  
    filter(TIPOBITO == "2") %>% 
    select(date_e = DTOBITO, 
           IDADE,
           SEXO) %>% 
    mutate(date_e = dmy(date_e),
           Year = year(date_e),
           Sex = recode(SEXO,
                        "0" = "UNK",
                        "1" = "m",
                        "2" = "f"),
           Age = case_when(IDADE <= 400 ~ "0",
                           IDADE > 400 & IDADE < 500 ~ str_sub(IDADE, 2, 3),
                           IDADE >= 500 & IDADE <= 600 ~ "100",
                           TRUE ~ "UNK")) %>% 
    group_by(Year, Sex, Age) %>% 
    summarize(Deaths = n(), .groups = "drop") %>% 
    spread(Sex, Deaths) %>% 
    replace_na(list(UNK = 0,
                    m = 0,
                    f = 0)) %>% 
    mutate(t = f + m + UNK) %>% 
    select(-UNK) %>% 
    gather(f, m, t, key = Sex, value = Deaths) %>% 
    group_by(Year, Sex, Age) %>% 
    summarize(Deaths = sum(Deaths), .groups = "drop")
}

dts <- 
  out %>% 
  bind_rows()

tot_age <- 
  dts %>% 
  group_by(Year, Sex) %>% 
  summarise(Deaths = sum(Deaths), .groups = "drop") %>% 
  mutate(Age = "TOT")

# rescaling age and sex
dts2 <- 
  dts %>% 
  filter(Age != "UNK") %>% 
  bind_rows(tot_age) %>% 
  group_by(Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>%
  group_by(Age, Year) %>%
  do(rescale_sex(chunk = .data)) %>%
  ungroup()
  
dts3 <- 
  dts2 %>% 
  mutate(Age = Age %>% as.double(),
         Country = "Brazil",
         Code = "BRA",
         Source = "brazil_sim") %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Source) %>% 
  arrange(Year, Sex, Age)

write_rds(dts3, "data_inter/brazil.rds")

