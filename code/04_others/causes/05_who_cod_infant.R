library(here)
library(readxl)
source(here("Code", "00_functions.R"))

p_scores <- 
  read_rds("Output/p_scores_excess_raw_data.rds") %>% 
  filter(Age == "Infant",
         Year == 2020,
         Sex == "t") %>% 
  select(Country, p_score)



db <- 
  read_csv("Data/WHO/icd_raw.csv",
           col_types = cols(.default = "c"))

cts20 <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  pull(Country) %>% 
  unique

cts20 <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  select(Country = name, IM_Frmat) %>% 
  unique() %>% 
  mutate(Country = str_replace(Country, "United Kingdom, ", ""),
         Country = ifelse(Country == "Czech Republic", "Czechia", Country)) %>% 
  arrange() %>% 
  left_join(p_scores)

write.excel(cts20)


lists <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  select(name, List) %>% 
  unique

age_inf <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  select(name, IM_Frmat) %>% 
  unique

mex <- 
  db %>% 
  filter(Year >= 2015,
         name == "Mexico")

vars_dts <- paste0("Deaths", 2:10)

db20 <- 
  db %>% 
  mutate(Year = Year %>% as.double()) %>% 
  filter(Country %in% cts20,
         Year >= 2015) %>% 
  select(Country = name, Year, Sex, List, Cause, c(vars_dts)) %>% 
  gather(c(vars_dts), key = Age, value = Deaths) %>% 
  replace_na(list(Deaths = 0)) %>% 
  mutate(Deaths = Deaths %>% as.double(), 
         Age = case_when(Age == 'Deaths2' ~ '0',
                         Age == 'Deaths3' ~ '1',
                         Age == 'Deaths4' ~ '1',
                         Age == 'Deaths5' ~ '1',
                         Age == 'Deaths6' ~ '1',
                         Age == 'Deaths7' ~ '5',
                         Age == 'Deaths8' ~ '10',
                         Age == 'Deaths9' ~ '15',
                         Age == 'Deaths10' ~ '20'),
         Age = Age %>% as.double(),
         Sex = case_when(Sex == 1 ~ "m",
                         Sex == 2 ~ "f",
                         TRUE ~ NA_character_)) %>% 
  group_by(Country, Year, List, Cause, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()


unique(db20$List)
test <- 
  db20 %>% 
  select(Country, List) %>% 
  unique()


# definition of leading causes of death according to Becker list 
# (who.int/bulletin/volumes/84/4/297.pdf)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
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
  b73 <- c(paste0("P", sprintf("%02d",00:96)))
  }

db20_2 <- 
  db20 %>% 
  filter(Cause != "AAA") %>% 
  mutate(Cause = str_sub(Cause, 1, 3),
         c_b = case_when(Cause %in% c(paste0("A", sprintf("%02d",00:99))) ~ "circ",
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
         Cause = paste0("c", sprintf("%02d", c_b))) %>% 
  group_by(Country, Year, Cause, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db20_3 <- 
  db20_2 %>% 
  complete(Country, Year, Age, Cause, fill = list(Deaths = 0))

# %>% 
#   group_by(Country, Year, Age) %>% 
#   mutate(prop_year = Deaths / sum(Deaths)) %>% 
#   ungroup() %>% 
#   group_by(Country, Age, Cause) %>% 
#   mutate(av_p = mean(prop_year)) %>% 
#   ungroup()
# %>% 
#   # mutate(Cause = ifelse(av_p < 0.005, "c88", Cause)) %>% 
#   group_by(Year, Age, Cause) %>% 
#   summarise(Deaths = sum(Deaths)) %>% 
#   ungroup()

db <- temp2

pred_this <- function(db){
  Year <- db %>% drop_na() %>% pull(Year)
  Deaths <- db %>% drop_na() %>% pull(Deaths)
  new <- tibble(Year = min(Year):2020)
  res <- predict(lm(Deaths ~ Year), new)
  out <- tibble(Year = min(Year):2020, 
                Deaths = as.numeric(res))
  return(out)
}

pred_this <- function(db){
  Year <- db %>% drop_na() %>% pull(Year)
  Deaths <- (db %>% drop_na() %>% pull(Deaths) + 1) %>% log()
  new <- tibble(Year = min(Year):2020)
  res <- (predict(lm(Deaths ~ Year), new) %>% exp()) - 1
  out <- tibble(Year = min(Year):2020, 
                Deaths = as.numeric(res))
  return(out)
}




ct <- "Austria"
a <- 0
c <- "c99"

cts <- unique(db20_3$Country)
as <- unique(db20_3$Age)
out <- list()

for(ct in cts){
  temp <- 
    db20_3 %>% 
    filter(Country == ct)
  
  for(a in as){
    
    temp0 <- 
      temp %>% 
      filter(Age == a)
    cs <- unique(temp0$Cause)
    
    for(c in cs){
      
      temp1 <-  
        temp0 %>% 
        filter(Cause == c)
      
      temp2 <- 
        temp1 %>% 
        filter(Year < 2020)
      
      # temp2 <- 
      #   spline_this(temp1, 1e-5) %>% 
      #   mutate(type = "spline")
      
      temp3 <- 
        pred_this(temp2) %>% 
        mutate(type = "linear")
      
      iter <- paste0(ct, "_", a, "_", c)
      cat(paste0(iter, "\n"))
      out[[iter]] <- 
        temp1 %>% 
        select(Year, Deaths) %>% 
        mutate(type = "observed") %>% 
        bind_rows(temp3) %>% 
        mutate(Country = ct,
               Age = a,
               Cause = c)
    }
  }
}

db20_4 <- 
  out %>% 
  bind_rows() %>% 
  mutate(Deaths = ifelse(Deaths >= 0, Deaths %>% round(), 0)) %>% 
  group_by(Country, Year, Age, type) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup()

# ~~~~~~~~~~~~~~~~
unique(db20_4$Age)  

diffs_2020 <- 
  db20_4 %>% 
  select(Country, Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(diff = observed - linear)

# contribution to negative changes in mortality (reduction)
neg_conts_2020 <- 
  diffs_2020 %>% 
  select(Country, Age, Cause, diff) %>% 
  filter(diff < 0) %>% 
  group_by(Country, Age) %>% 
  mutate(cont = diff/sum(diff)) %>% 
  ungroup()

# contribution to positive changes in mortality (increase)
pos_conts_2020 <- 
  diffs_2020 %>% 
  select(Country, Age, Cause, diff) %>% 
  filter(diff > 0) %>% 
  group_by(Country, Age) %>% 
  mutate(cont = diff/sum(diff)) %>% 
  ungroup()

# selecting leading causes contributing to at least 60%
neg_conts_2020_filter <- 
  neg_conts_2020 %>% 
  arrange(Country, Age, -cont) %>% 
  group_by(Country, Age) %>% 
  mutate(cum_cont = cumsum(cont),
         id = 1:n(),
         rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
  ungroup() %>% 
  mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
  group_by(Country, Age, Cause) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup() %>% 
  group_by(Country, Age) %>% 
  mutate(cont = diff / sum(diff)) %>% 
  ungroup() %>% 
  mutate(cont_type = "negative")

pos_conts_2020_filter <- 
  pos_conts_2020 %>% 
  arrange(Country, Age, -cont) %>% 
  group_by(Country, Age) %>% 
  mutate(cum_cont = cumsum(cont),
         id = 1:n(),
         rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
  ungroup() %>% 
  mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
  group_by(Country, Age, Cause) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup() %>% 
  group_by(Country, Age) %>% 
  mutate(cont = diff / sum(diff)) %>% 
  ungroup() %>% 
  mutate(cont_type = "positive")

pos_conts_2020 %>% 
  filter(Country == "Austria") %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = fct_reorder(Cause, -cont)), 
           stat = "identity", 
           position= "fill")+
  coord_cartesian(expand = 0)

# ggsave(paste0("Figures/causes/cont_cause_brazil_young.png"), dpi = 600)




conts_2020_filter <- 
  bind_rows(pos_conts_2020_filter,
            neg_conts_2020_filter) %>%
  mutate(cont = ifelse(cont_type == "positive", cont, -cont))


write_rds(conts_2020_filt, "Output/causes_changes_2020_who_mort_db.rds")


causes_cods <- read_xlsx("Data/becker_codes.xlsx") 

causes_cods2 <- 
  causes_cods %>% 
  mutate(Cause = paste0("c", id)) %>% 
  select(Cause, causes, codes)


db_cs <- 
  conts_2020_filter %>% 
  left_join(causes_cods2) %>% 
  mutate(Cause_des = case_when(Cause == 'c5' ~ 'Meningitis', 
                               Cause == 'c18' ~ 'Cancer uterus', 
                               Cause == 'c24' ~ 'Cancer lymphoid', 
                               Cause == 'c28' ~ 'Dehydratation',
                               Cause == 'c36' ~ 'Cardiorespiratory', 
                               Cause == 'c46' ~ 'Influenza and Pneumonia', 
                               Cause == 'c48' ~ 'Pulmonary oedema', 
                               Cause == 'c49' ~ 'Respiratory failure', 
                               Cause == 'c53' ~ 'Urinary syst', 
                               Cause == 'c55' ~ 'Perinatal', 
                               Cause == 'c56' ~ 'Congenital', 
                               Cause == 'c57' ~ 'Traffic accidents', 
                               Cause == 'c6' ~ 'Septicaemia', 
                               Cause == 'c60' ~ 'Accidental drowning', 
                               Cause == 'c63' ~ 'Suicide', 
                               Cause == 'c64' ~ 'Homicide', 
                               Cause == 'c72' ~ 'COVID-19', 
                               Cause == 'c88' ~ 'Remainder', 
                               Cause == 'c99' ~ 'Ill-defined'))

ext <- c("c57", "c60", "c63", "c64")
res_inf <- c("c6", "c46", "c48", "c49", "c71")

css <- 
  db_cs %>% 
  select(Cause, causes) %>% 
  group_by(Cause) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(-n)

write.excel(css)

cols <- 
  c(
    # external
    "Traffic accidents" = "#660708", 
    "Accidental drowning" = "#a4161a", 
    "Suicide" = "#ba181b", 
    "Homicide" = "#e5383b",
    # respiratory and infectious
    "Meningitis" = "#007f5f",
    "Septicaemia" = "#2b9348", 
    "Influenza and Pneumonia" = "#80b918", 
    "Pulmonary oedema" = "#bfd200", 
    "Respiratory failure" = "#eeef20", 
    # "Cardiorespiratory" = "#eeef20",
    # congenital / perinatal
    "Perinatal" = "#7b2cbf",
    "Congenital" = "#c77dff",
    # others
    # cancer
    "Cancer lymphoid" = "#01497c",
    # 'Cancer uterus' = "#2a6f97",
    # 'Urinary syst' = "#468faf",
    # 'Dehydratation' = "#89c2d9",
    # rest
    "Ill-defined" = "#936639",
    "Remainder" = "grey"
  )


levs <- 
  c(
    # external
    "Traffic accidents", 
    "Accidental drowning", 
    "Suicide", 
    "Homicide",
    # respiratory and infectious
    "Meningitis",
    "Septicaemia", 
    "Influenza and Pneumonia", 
    "Pulmonary oedema", 
    "Respiratory failure", 
    "Cardiorespiratory",
    # congenital / perinatal
    "Perinatal",
    "Congenital",
    # others
    # cancer
    "Cancer lymphoid",
    'Cancer uterus',
    'Urinary syst',
    'Dehydratation',
    # rest
    "Ill-defined",
    "Remainder"
  )

db_cs %>% 
  mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = Cause_des), 
           stat = "identity", 
           position="fill")+
  coord_cartesian(expand = 0)+
  scale_fill_manual(values = cols)+
  facet_grid(~ Country)+
  labs(fill = "Cause",
       x = "Age group",
       y = "Neg. contribution")+
  theme(legend.key.size = unit(0.2, "cm"))

ggsave("Figures/causes/composition_change_who.png", width = 8, height = 4.09)


db_cs %>% 
  filter(Age == 0) %>% 
  mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = Cause_des), 
           stat = "identity", 
           position="fill")+
  coord_cartesian(expand = 0)+
  scale_fill_manual(values = cols)+
  facet_grid(~ Country)+
  labs(fill = "Cause",
       x = "Age group",
       y = "Neg. contribution")+
  theme(legend.key.size = unit(0.2, "cm"))


db_cs %>% 
  filter(Age == 15) %>% 
  mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = Cause_des), 
           stat = "identity", 
           position="fill")+
  coord_cartesian(expand = 0)+
  scale_fill_manual(values = cols)+
  facet_grid(~ Country)+
  labs(fill = "Cause",
       x = "Age group",
       y = "Neg. contribution")+
  theme(legend.key.size = unit(0.2, "cm"))





