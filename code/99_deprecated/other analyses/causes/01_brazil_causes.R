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
# time_units <- 
#   c("0" = "mins",
#     "1" = "hrs", 
#     "2" = "days", 
#     "3" = "mts", 
#     "4" = "yrs",
#     "5" = ">100")
# 
# links <- 
#   c(paste0("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_",2015:2019,".csv"),
#     "https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31/download/dobrano_.csv")
# 
# # links <- unzip(here("Data", "Brazil", "brazil_deaths.zip"), list = TRUE)
# # 
# # test <- 
# #   read_csv2(unzip(here("Data", "Brazil", "brazil_deaths.zip"), links[1,1]))
# 
# links <- tibble(Year = 2015:2020,
#                 File = paste0(here("Data", "Brazil"), "/Mortalidade_Geral_", 2015:2020, ".csv"))
# 
# out <- list()
# for (i in 1:nrow(links)){
#   
#   cat(links[i,1] %>% as.character(),"\n")
#   out[[i]] <- read_delim(links[i,2] %>% as.character(), 
#                          delim = ";",
#                          col_types = cols(.default = "c")) %>%  
#     select(age_val = IDADE, Cause = LINHAA, Date = DTOBITO, 
#            Date_reg = DTRECEBIM, diff = DIFDATA) %>% 
#     mutate(Date = dmy(Date),
#            Date_reg = dmy(Date_reg),
#            Date_reg2 = Date + days(diff),
#            time_unit = str_sub(age_val, 1, 1),
#            time_unit = recode(time_unit,
#                               !!!time_units),
#            age_val = str_sub(age_val, 2, 3) %>% as.integer(),
#            Source = links[i,1] %>% as.character())
# }
# 
# db_bra_0 <-
#   out %>% 
#   bind_rows()
# 
# write_rds(db_bra_0, "Output/brazil_micro_2015_2020.rds")
db_bra_0 <- read_rds("Output/brazil_micro_2015_2020.rds")

db_bra1 <- 
  db_bra_0 %>% 
  mutate(Age = case_when(time_unit != "yrs" ~ 0,
                         time_unit == "yrs" & age_val >= 1 & age_val <= 4 ~ 1,
                         time_unit == "yrs" & age_val >= 5 & age_val <= 9 ~ 5,
                         time_unit == "yrs" & age_val >= 10 & age_val <= 14 ~ 10,
                         time_unit == "yrs" & age_val >= 15 & age_val <= 24 ~ 15,
                         TRUE ~ 99),
         Cause = str_sub(Cause, 2, 4),
         Year = year(Date),
         Date_limit = make_date(d = 3, m = 3, y = Year + 1),
         Include = ifelse(Date_reg2 <= Date_limit, 1, 0)) %>% 
  filter(Include == 1) %>% 
  group_by(Age, Year, Cause) %>% 
  summarise(Deaths = n()) %>% 
  ungroup()

# definition of leading causes of death according to Becker list (who.int/bulletin/volumes/84/4/297.pdf)
b1 <- c(paste0("A", sprintf("%02d",0:9)))
b2 <- c(paste0("A", 15:19))
b3  <- c('A20', 'A44', paste0("A", sprintf("%02d",75:79)), paste0("A", sprintf("%02d",82:84)), 'A85', paste0("A", sprintf("%02d",90:96)), 'A98', paste0("A", sprintf("%02d",50:57)))
b4  <- c(paste0("A", sprintf("%02d",33:37)), 'A80', 'B01', 'B05', 'B06', 'B15', 'B16', 'B17', 'B18', 'B19', 'B26')
b5  <- c('A39', 'A87', paste0("G", sprintf("%02d",0:3)))
b6 <- c(paste0("A", 40:41))
b7  <- c(paste0("B", sprintf("%02d",20:24)))
b8  <- 'C15'
b9  <- 'C16'
b10  <- c(paste0("C", sprintf("%02d",18:21)))
b11  <- 'C22'
b12  <- c('C23', 'C24')
b13  <- 'C25'
b14  <- 'C32'
b15  <- c('C33', 'C34')
b16  <- c('C43', 'C44')
b17  <- 'C50'
b18  <- c(paste0("C", sprintf("%02d", 53:55)))
b19  <- 'C56'
b20  <- 'C61'
b21  <- 'C64'
b22  <- 'C67'
b23  <- 'C71'
b24 <- c(paste0("C", 81:96))
b25  <- c(paste0("D", sprintf("%02d",00:48)))
b26  <- c(paste0("E", sprintf("%02d",10:14)))
b27 <- c(paste0("D", 50:53), paste0("E", 40:64))
b28  <- c(paste0("E", sprintf("%02d",86:87)))
b29  <- c('F01', 'F03', 'G30')
b30  <- c(paste0("F", sprintf("%02d",10:19)))
b31  <- 'G20'
b32  <- c('G40', 'G41')
b33  <- c(paste0("I", sprintf("%02d",05:09)))
b34  <- c(paste0("I", sprintf("%02d",10:15)))
b35  <- c(paste0("I", sprintf("%02d",20:25)))
b36  <- c(paste0("I", sprintf("%02d",26:28)))
b37  <- c(paste0("I", sprintf("%02d",34:38)))
b38  <- 'I42'
b39  <- 'I46'
b40  <- c(paste0("I", sprintf("%02d",47:49)))
b41 <- c(paste0("I", 50:51))
b42  <- c(paste0("I", 60:69))
b43  <- 'I70'
b44  <- 'I71'
b45 <- c(paste0("J", sprintf("%02d",00:06)), paste0("J", 20:22))
b46 <- c(paste0("J", 10:18))
b47 <- c(paste0("J", 40:47))
b48 <- c(paste0("J", sprintf("%02d",80:84)))
b49 <- c(paste0("J", 96))
b50  <- c(paste0("K", sprintf("%02d",35:46)), 'K56')
b51  <- c(paste0("K", sprintf("%02d",70:76)))
b52  <- c(paste0("M", sprintf("%02d",00:99)))
b53  <- c(paste0("N", sprintf("%02d",00:39)))
b54  <- c(paste0("O", sprintf("%02d",00:99)))
b55  <- c(paste0("P", sprintf("%02d",00:96)))
b56 <- c(paste0("Q", sprintf("%02d",00:99)))
b57 <- c(paste0("V", sprintf("%02d",01:89)))
b58 <- c(paste0("W", sprintf("%02d",01:19)))
b59  <- c(paste0("W", sprintf("%02d",32:34)))
b60 <- c(paste0("W", 65:74))
b61 <- c(paste0("W", 75:84))
b62  <- 'X40-X49'
b63  <- 'X60-X84'
b64  <- 'X85-Y09'
b65  <- 'Y10-Y34'
b99 <- c(paste0("R", sprintf("%02d",00:08)), paste0("R", sprintf("%02d",10:99)))
bcv <- c("U07")
crs <- 'R09'
inj <- c(paste0("S", sprintf("%02d",00:99)), paste0("T", sprintf("%02d",00:99)))

age <- 5

db_child <- 
  db_bra1 %>% 
  # filter(Age == age) %>% 
  drop_na(Cause) %>% 
  mutate(c_b = case_when(Cause %in% b1 ~ 1,
                         Cause %in% b2 ~ 2,
                         Cause %in% b3 ~ 3,
                         Cause %in% b4 ~ 4,
                         Cause %in% b5 ~ 5,
                         Cause %in% b6 ~ 6,
                         Cause %in% b7 ~ 7,
                         Cause %in% b8 ~ 8,
                         Cause %in% b9 ~ 9,
                         Cause %in% b10 ~ 10,
                         Cause %in% b11 ~ 11,
                         Cause %in% b12 ~ 12,
                         Cause %in% b13 ~ 13,
                         Cause %in% b14 ~ 14,
                         Cause %in% b15 ~ 15,
                         Cause %in% b16 ~ 16,
                         Cause %in% b17 ~ 17,
                         Cause %in% b18 ~ 18,
                         Cause %in% b19 ~ 19,
                         Cause %in% b20 ~ 20,
                         Cause %in% b21 ~ 21,
                         Cause %in% b22 ~ 22,
                         Cause %in% b23 ~ 23,
                         Cause %in% b24 ~ 24,
                         Cause %in% b25 ~ 25,
                         Cause %in% b26 ~ 26,
                         Cause %in% b27 ~ 27,
                         Cause %in% b28 ~ 28,
                         Cause %in% b29 ~ 29,
                         Cause %in% b30 ~ 30,
                         Cause %in% b31 ~ 31,
                         Cause %in% b32 ~ 32,
                         Cause %in% b33 ~ 33,
                         Cause %in% b34 ~ 34,
                         Cause %in% b35 ~ 35,
                         Cause %in% b36 ~ 36,
                         Cause %in% b37 ~ 37,
                         Cause %in% b38 ~ 38,
                         Cause %in% b39 ~ 39,
                         Cause %in% b40 ~ 40,
                         Cause %in% b41 ~ 41,
                         Cause %in% b42 ~ 42,
                         Cause %in% b43 ~ 43,
                         Cause %in% b44 ~ 44,
                         Cause %in% b45 ~ 45,
                         Cause %in% b46 ~ 46,
                         Cause %in% b47 ~ 47,
                         Cause %in% b48 ~ 48,
                         Cause %in% b49 ~ 49,
                         Cause %in% b50 ~ 50,
                         Cause %in% b51 ~ 51,
                         Cause %in% b52 ~ 52,
                         Cause %in% b53 ~ 53,
                         Cause %in% b54 ~ 54,
                         Cause %in% b55 ~ 55,
                         Cause %in% b56 ~ 56,
                         Cause %in% b57 ~ 57,
                         Cause %in% b58 ~ 58,
                         Cause %in% b59 ~ 59,
                         Cause %in% b60 ~ 60,
                         Cause %in% b61 ~ 61,
                         Cause %in% b62 ~ 62,
                         Cause %in% b63 ~ 63,
                         Cause %in% b64 ~ 64,
                         Cause %in% b65 ~ 65,
                         Cause %in% inj ~ 70,
                         Cause %in% crs ~ 36,
                         Cause %in% bcv ~ 72,
                         Cause %in% b99 ~ 99,
                         TRUE ~ 88),
         Cause = paste0("c", c_b)) %>% 
  group_by(Year, Age) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age < 99)

db_child2 <- 
  db_child %>% 
  group_by(Year, Age, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  complete(Year, Age, Cause, fill = list(Deaths = 0)) %>% 
  group_by(Year, Age) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Cause, Age) %>% 
  mutate(trend = Deaths / mean(Deaths),
         ch = Deaths / lag(Deaths),
         ch_prop_year = prop_year / lag(prop_year)) %>% 
  ungroup() 

unique(db_child2$Age)  

# extrapolating deaths
# ~~~~~~~~~~~~~~~~~~~~

db_child3 <- 
  db_child2 %>% 
  select(Year, Age, Cause, Deaths, prop_year) %>% 
  group_by(Age, Cause) %>% 
  mutate(av_p = mean(prop_year)) %>% 
  ungroup() %>% 
  # mutate(Cause = ifelse(av_p < 0.005, "c88", Cause)) %>% 
  group_by(Year, Age, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

unique(db_child3$Cause)

as <- unique(db_child3$Age)
out <- list()

# a <- 0
# c <- "c46"

for(a in as){
  
  temp <- 
    db_child3 %>% 
    filter(Age == a)
  cs <- unique(temp$Cause)
  
  for(c in cs){
    
    temp1 <- 
      temp %>% 
      filter(Year < 2020,
             Cause == c)
    temp2 <- 
      spline_this(temp1, 1e-5) %>% 
      mutate(type = "spline")
    
    temp3 <- pred_this(temp1) %>% 
      mutate(type = "linear")
    
    iter <- paste0(a, "_", c)
    
    out[[iter]] <- 
      temp %>% 
      filter(Cause == c) %>% 
      select(Year, Deaths) %>% 
      mutate(type = "observed") %>% 
      bind_rows(temp2,
                temp3) %>% 
      mutate(Age = a,
             Cause = c)
  }
}

db_child4 <- 
  out %>% 
  bind_rows() %>% 
  group_by(Year, Age, type) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup()
  
unique(db_child4$Age)  

# db_child4 %>% 
#   filter(Cause == "c6",
#          Age == 15) %>% 
#   ggplot()+
#   geom_line(aes(Year, Deaths, col = type))

props_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, prop_year, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, prop_year) %>% 
  mutate(ratio_prop = observed / linear)

deaths_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(ratio_prop = observed / linear)

sum_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  group_by(Age, type) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

diffs_2020_bra <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(diff = observed - linear)

conts_2020_bra <- 
  diffs_2020_bra %>% 
  select(Age, Cause, diff) %>% 
  filter(diff < 0) %>% 
  group_by(Age) %>% 
  mutate(cont = diff/sum(diff)) %>% 
  ungroup()

conts_2020_plus <- 
  diffs_2020_bra %>% 
  select(Age, Cause, diff) %>% 
  filter(diff > 0) %>% 
  group_by(Age) %>% 
  mutate(cont = diff/sum(diff)) %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  arrange(Age, -cont) %>% 
  mutate(cum_cont = cumsum(cont),
         id = 1:n(),
         rest = ifelse(lag(cum_cont) < 0.8 | id == 1, 0, 1)) %>% 
  ungroup() %>% 
  mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
  group_by(Age, Cause) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  mutate(cont = diff / sum(diff))


conts_2020_filt_bra <- 
  conts_2020_bra %>% 
  arrange(Age, -cont) %>% 
  group_by(Age) %>% 
  mutate(cum_cont = cumsum(cont),
         id = 1:n(),
         rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
  ungroup() %>% 
  mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
  group_by(Age, Cause) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  mutate(cont = diff / sum(diff))

conts_2020_filt %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = fct_reorder(Cause, -cont)), 
           stat = "identity", 
           position= "fill")+
  coord_cartesian(expand = 0)

ggsave(paste0("Figures/causes/cont_cause_brazil_young.png"), dpi = 600)

write_rds(conts_2020_filt_bra, "Output/causes_changes_2020_brazil.rds")



