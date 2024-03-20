library(here)
source(here("Code", "00_functions.R"))
library(haven)
db_cr <- read_sav("Data/Costa Rica/Defu2015-2020.OPS.sav") %>% 
  as_tibble()

unique(db_cr$sexo)
unique(db_cr$codigo)
unique(db_cr$edad) %>% sort
unique(db_cr$causa) %>% sort


db_cr1 <- 
  db_cr %>% 
  mutate(edad = edad %>% as.double(),
         codigo = codigo %>% as.integer(),
         Age = case_when(codigo %in% as.character(1:4) ~ 0,
                         codigo == "5" & edad %in% 1:4 ~ 1,
                         codigo == "5" & edad >= 5 & edad <= 9 ~ 5,
                         codigo == "5" & edad >= 10 & edad <= 14 ~ 10,
                         codigo == "5" & edad >= 15 & edad <= 24 ~ 15,
                         TRUE ~ 99),
         Cause = str_sub(causamuer, 1, 3),
         Year = anodef %>% as.double()) %>% 
  select(Age, Year, Cause) %>% 
  group_by(Age, Year, Cause) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  filter(Year >= 2015)


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

db_cr2 <- 
  db_cr1 %>% 
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
  group_by(Year, Age, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Year, Age) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age < 99) %>% 
  select(Year, Age, Cause, Deaths)


unique(db_cr2$Cause)

cs_all_years <- 
  db_cr2 %>% 
  group_by(Age, Cause) %>% 
  summarise(yrs = n()) %>% 
  ungroup() %>% 
  filter(yrs == 6) %>% 
  select(Age, Cause) %>% 
  mutate(keep = 1)

db_cr3 <- 
  db_cr2 %>%
  left_join(cs_all_years) %>% 
  drop_na()
  
as <- unique(db_cr3$Age)
out <- list()

# a <- 0
# c <- "c46"

for(a in as){
  
  temp <- 
    db_cr3 %>% 
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

db_cr4 <- 
  out %>% 
  bind_rows() %>% 
  group_by(Year, Age, type) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup()

props_2020 <- 
  db_cr4 %>% 
  select(Year, Age, Cause, prop_year, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, prop_year) %>% 
  mutate(ratio_prop = observed / linear)

deaths_2020 <- 
  db_cr4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(ratio_prop = observed / linear)

sum_2020 <- 
  db_cr4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  group_by(Age, type) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

diffs_2020 <- 
  db_cr4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(diff = observed - linear)

conts_2020 <- 
  diffs_2020 %>% 
  select(Age, Cause, diff) %>% 
  filter(diff < 0) %>% 
  group_by(Age) %>% 
  mutate(cont = diff/sum(diff)) %>% 
  ungroup()

conts_2020_plus <- 
  diffs_2020 %>% 
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


conts_2020_filt <- 
  conts_2020 %>% 
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
           position="fill")+
  coord_cartesian(expand = 0)

ggsave(paste0("Figures/causes/cont_cause_costa_rica_young.png"), dpi = 600)

write_rds(conts_2020_filt, "Output/causes_changes_2020_costa_rica.rds")






