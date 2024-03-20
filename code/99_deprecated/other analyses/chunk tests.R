library(foreign)
t20 <- read.dbf("Data/Brazil/DOBR20DA.DBF")
t21 <- read.dbf("Data/Brazil/DOBR21DA.DBF")

fet20 <- 
  t20 %>% 
  filter(TIPOBITO == 1) %>% 
  as_tibble() %>% 
  mutate(date = dmy(DTOBITO),
         month = month(date),
         year = year(date),
         y_m = paste(year, month, sep = "_"))

fet20_2 <- 
  fet20 %>% 
  group_by(y_m) %>% 
  summarise(d = n())

fet20_2 %>% 
  summarise(sum(d))

fet21 <- 
  t21 %>% 
  filter(TIPOBITO == 1) %>% 
  as_tibble() %>% 
  mutate(date = dmy(DTOBITO),
         month = month(date),
         year = year(date),
         y_m = paste(year, month, sep = "_"))

fet21_2 <- 
  fet21 %>% 
  group_by(y_m) %>% 
  summarise(d = n())





library(haven)

path = file.path("C:/", "Folder", "dataset.sav")
dataset = read_sav(path)


cr <- read.spss("Data/Costa Rica/Defu2015-2020.OPS.sav")

library(haven)
cr2 <- read_sav("Data/Costa Rica/Defu2015-2020.OPS.sav")

unique(cr2$codigo)
unique(cr2$sexo)


y2019 <- read_delim(link2019, delim = ";")



link2019 <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2019.csv"
link2018 <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2018.csv"
link2017 <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2017.csv"
link2016 <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2016.csv"
link2015 <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2015.csv"

years <- 2015:2019
out <- list()
for (i in 1:length(years)){
  cat(i,"\n")
  link <- paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_",years[i],".csv")
  out[[i]] <- read_delim(link, delim = ";") %>% 
    group_by(IDADE, SEXO) %>% 
    summarize(Deaths = n(), .groups = "drop") %>% 
    mutate(age_type = substr(IDADE, 1,1),
           Age = substr(IDADE, 2,3),
           Age = case_when(age_type %in% c("0","9")~"UNK",
                           is.na(age_type)~"UNK",
                           age_type %in% c("1","2","3")~"00",
                           age_type == "5" ~ "100",
                           TRUE ~ Age),
           Sex = recode(SEXO,
                        "0" = "UNK",
                        "1" = "m",
                        "2" = "f"))%>% 
    group_by(Sex, Age) %>% 
    summarize(Deaths = sum(Deaths), .groups = "drop") %>% 
    pivot_wider(names_from = Sex, values_from = Deaths) %>%
    mutate(UNK = ifelse(is.na(UNK),0, UNK),
           t=f+m+UNK) %>% 
    select(-UNK) %>% 
    adorn_totals("row",name = "TOT") %>% 
    dplyr::filter(Age != "UNK") %>% 
    pivot_longer(f:t, names_to = "Sex", values_to = "Deaths") %>% 
    mutate(Age = ifelse(Age == "TOT", "TOT", Age %>% as.integer() %>% as.character())) %>% 
    arrange(Sex, Age) %>% 
    mutate(Country = "Brazil",
           Code = "BRA",
           Year = years[i],
           Source = "country_public") %>% 
    select(Country, Code, Year, Sex, Age, Deaths, Source)
  
}

db_bra <-
  out %>% 
  bind_rows() %>% 
  bind_rows(BR2020) %>% 
  arrange(Year, Sex, as.integer(Age)) 


test <- read_csv2("Data/Brazil/Mortalidade_Geral_2015.csv")
unique(test$TIPOBITO)
