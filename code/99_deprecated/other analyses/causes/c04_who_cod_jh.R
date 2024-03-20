library(readxl)
source("Code/00_functions.R")

db <- 
  read_csv("Data/WHO/icd_raw.csv",
           col_types = cols(.default = "c"))

cts20 <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  select(Country = name, IM_Frmat) %>% 
  unique() %>% 
  mutate(Country = str_replace(Country, "United Kingdom, ", ""),
         Country = ifelse(Country == "Czech Republic", "Czechia", Country)) %>% 
  arrange() 

cts20 <- 
  db %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  pull(Country) %>% 
  unique

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

bh <- 
  db %>% 
  filter(Year >= 2015,
         name == "Bosnia and Herzegovina")

vars_dts <- c(paste0("Deaths", 2:10), paste0("IM_Deaths", 1:4))

# unique(db2$Age)


db1 <- 
  db %>% 
  mutate(Year = Year %>% as.double()) %>% 
  filter(Country %in% cts20,
         Year >= 2015)

cts_inf <- 
  db1 %>% 
  select(IM_Frmat, name) %>% 
  unique() %>% 
  group_by(name) %>% 
  summarise(IM_Frmat = paste(IM_Frmat, collapse = ", ")) %>% 
  ungroup() %>% 
  filter(IM_Frmat == "01") %>% 
  pull(name)

db2 <- 
  db %>% 
  mutate(Year = Year %>% as.double()) %>% 
  filter(Country %in% cts20,
         Year >= 2015) %>% 
  select(Country = name, Year, Sex, Cause, c(vars_dts)) %>% 
  gather(c(vars_dts), key = Age, value = Deaths) %>% 
  replace_na(list(Deaths = 0)) %>% 
  mutate(Deaths = Deaths %>% as.double(), 
         Age = case_when(Age == 'IM_Deaths1' ~ "neo",
                         Age == 'IM_Deaths2' ~ "neo",
                         Age == 'IM_Deaths3' ~ "neo",
                         Age == 'IM_Deaths4' ~ "post_neo",
                         Age == 'Deaths2' ~ '0',
                         Age == 'Deaths3' ~ '1',
                         Age == 'Deaths4' ~ '1',
                         Age == 'Deaths5' ~ '1',
                         Age == 'Deaths6' ~ '1',
                         Age == 'Deaths7' ~ '5',
                         Age == 'Deaths8' ~ '10',
                         Age == 'Deaths9' ~ '15',
                         Age == 'Deaths10' ~ '20'),
         Sex = case_when(Sex == 1 ~ "m",
                         Sex == 2 ~ "f",
                         TRUE ~ NA_character_)) %>% 
  mutate(age_type = ifelse(Age %in% c("neo", "post_neo"), 
                           "infant", "young")) %>% 
  filter(age_type == "young" | (age_type == "infant" & Country %in% cts_inf)) %>% 
  group_by(Country, Year, Cause, Age, age_type) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  rename(country = Country,
         year = Year,
         cause = Cause,
         age = Age,
         dts = Deaths) %>% 
  mutate(country = recode(country,
                          "Czech Republic" = "Czechia",
                          "United Kingdom, England and Wales" = "England and Wales",
                          "United Kingdom, Scotland" = "Scotland"))


unique(db2$country)

cods <- function(l, n1, n2){
  c(paste0(l, sprintf("%02d",n1:n2)))
}
cods3 <- function(l, n1, n2){
  c(paste0(l, sprintf("%03d",n1:n2)))
}

{ # Definition of causes according to JH groups for child mortality
  # comm gr1
  c0010 <- c(cods("A", 00, 99),
             cods("B", 00, 99),
             cods("D", 50, 53),
             cods("E", 00, 02),
             cods("E", 40, 64),
             "G00",
             cods("G", 3, 4),
             cods("H", 65, 66),
             cods("J", 00, 22),
             "J85",
             "N30", "N34",
             cods("N", 70, 73),
             cods("O", 00, 99),
             cods("P", 00, 96),
             "U04")
  
  c0010_4 <- c("D649", "N390")
  # c0010_exc4 <- c("A480", "A483")
  
  c0020 <- cods("B", 20, 24)
  c0030 <- cods("A", 00, 09)
  c0040 <- "A37"
  c0050 <- cods("A", 33, 35)
  c0060 <- "B05"
  c0070 <- c("A39", cods("A", 83, 87), "G00", "G03", "G04")
  c0071 <- c(cods("B", 50, 54))
  c0071_4 <- c("P373", "P374")
  
  c0080 <- c("H65", "H66", cods("J", 00, 22), "J85", "P23")
  
  c0090 <- c("P07", "P22", cods("P", 25, 28), "P52", "P77")
  c0090_4 <- c("P010", "P011", "P612")
  
  c0091 <- cods("P", 00, 96)
  
  c0100 <- c("P03", cods("P", 10, 15), cods("P", 20, 21), "P24", "P50", 
             cods("P", 90, 91))
  c0100_4 <- c(cods3("P", 017, 021), cods3("P", 024, 026))
  
  c0110 <- cods("P", 35, 39)
  c0110_exc4 <- c("P373", "P374")
  
  c0120 <- "other gr1"
  
  
  # gr 2 noncom
  c0130 <- c(cods("C", 00, 97), 
             cods("D", 00, 48),
             cods("D", 55, 64),
             cods("D", 65, 89),
             cods("E", 03, 34),
             cods("E", 65, 88),
             cods("F", 01, 99),
             cods("G", 06, 98),
             cods("H", 00, 61),
             cods("H", 68, 93),
             cods("I", 00, 99),
             cods("J", 30, 84),
             cods("J", 86, 98),
             cods("K", 00, 92),
             cods("L", 00, 98),
             cods("M", 00, 99),
             cods("N", 00, 28),
             cods("N", 31, 32),
             cods("N", 35, 64),
             cods("N", 75, 98),
             cods("Q", 00, 99))
  
  c0130_exc4 <- c("D649", "N390")
  
  c0140 <- cods("Q", 0, 99)
  c0141 <- cods("J", 40, 44)
  c0150 <- "other nonc"
  
  # injuries
  c0160 <- c(cods("V", 0, 99), 
             cods("W", 0, 99), 
             cods("X", 0, 99), 
             cods("Y", 0, 89))
  
  # ill-defined
  c0900 <- c(cods("R", 0, 99), cods("U", 0, 99))
  c0900_exc4 <- c("U071", "U072")
  
  # covid
  c_cov <- c("U071", "U072")
}


unique(db2$cause)
db3 <- 
  db2 %>% 
  filter(cause != "AAA") %>% 
  mutate(cause3 = str_sub(cause, 1, 3),
         cause4 = str_sub(cause, 1, 4),
         g_jh = case_when(
           # gr 1
           cause3 %in% c0010 | cause4 %in% c0010_4 ~ "gr1",
           # gr 2
           (cause3 %in% c0130 & !cause4 %in% c0130_exc4) ~ "gr2",
           cause3 %in% c0160 ~ "gr3",
           cause3 %in% c0900 ~ "gr4",
           cause3 %in% c_cov ~ "gr5",
           
           TRUE ~ "residual_gr"),
         
         c_jh = case_when(
           # group 1 110
           cause3 %in% c0020 ~ "c020",
           cause3 %in% c0030 ~ "c030",
           cause3 %in% c0040 ~ "c040",
           cause3 %in% c0050 ~ "c050",
           cause3 %in% c0060 ~ "c060",
           cause3 %in% c0070 ~ "c070",
           cause3 %in% c0071 | cause4 %in% c0071_4 ~ "c071",
           cause3 %in% c0080 ~ "c080",
           cause3 %in% c0090 | cause4 %in% c0090_4 ~ "c090",
           # cause3 %in% c0091 ~ "c091",
           cause3 %in% c0100 | cause4 %in% c0100_4 ~ "c100",
           cause3 %in% c0110 & !cause4 %in% c0110_exc4 ~ "c110",
           
           # group 2 130
           cause3 %in% c0140 ~ "c140",
           cause3 %in% c0141 ~ "c141",

           # group 3. Injuries 160
           cause3 %in% c0160 ~ "c160",
           
           # group 4. Ill-defined 900
           cause3 %in% c0900 & !cause4 %in% c0900_exc4 ~ "c900",
           
           # group 5. covid
           cause4 %in% c_cov ~ "covid",
           
           TRUE ~ "other"),
         # residuals
         
         c_jh = case_when(
           # residual gr1: c120
           g_jh %in% "gr1" & c_jh == "other" ~ "c120_residual_gr1",
           # residual gr2: c120
           g_jh %in% "gr2" & c_jh == "other" ~ "c150_residual_gr2",
           TRUE ~ c_jh)) 


others <-
  db3 %>%
  filter(c_jh == "other") %>%
  select(cause3, cause4, g_jh, c_jh) %>%
  group_by(cause3, cause4, g_jh, c_jh) %>%
  summarise(n = n()) %>%
  ungroup()

resids <-
  db3 %>%
  filter(str_sub(c_jh, 6, 9) == "resi") %>%
  select(cause3, cause4, g_jh, c_jh) %>%
  group_by(cause3, cause4, g_jh, c_jh) %>%
  summarise(n = n()) %>%
  ungroup()

db4 <- 
  db3 %>% 
  group_by(country, year, age, age_type, c_jh, g_jh) %>% 
  summarise(dts = n()) %>% 
  ungroup() %>% 
  # excluding deaths recorded as COVID-19
  filter(!c_jh %in% 'covid')


# groups causes
gr1 <- c("c020", "c030", "c040", "c050", "c060", "c070", "c071", "c080", 
  "c090", "c091", "c100", "c110", "c120_residual_gr1")
gr2 <- c("c140", "c141", "c150_residual_gr2")
gr3 <- "c160"
gr4 <- "c900"
gr5 <- "covid"

# complete all observations with zeros
inf <- 
  db4 %>% 
  filter(age_type == "infant") %>% 
  complete(country, year, age_type, age, c_jh, fill = list(dts = 0))

unique(inf$age)

yng <- 
  db4 %>% 
  filter(age_type == "young") %>% 
  complete(country, year, age_type, age, c_jh, fill = list(dts = 0))

unique(yng$age)

db5 <- 
  bind_rows(inf, yng) %>% 
  mutate(g_jh = case_when(c_jh %in% gr1 ~ "gr1",
                          c_jh %in% gr2 ~ "gr2",
                          c_jh %in% gr3 ~ "gr3",
                          c_jh %in% gr4 ~ "gr4",
                          c_jh %in% gr5 ~ "gr5")) 

# deaths recorded as covid ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_covid <- 
  db3 %>% 
  filter(c_jh %in% 'covid') %>% 
  select(-cause, -cause3, -cause4)

# grouping causes that do not make at least 5% on average de contribution to 
# all cause mortality
unique(db5$age)

dts_causes <- 
  db5 %>% 
  group_by(country, age, year) %>% 
  mutate(prop = dts / sum(dts)) %>% 
  group_by(country, age, c_jh) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  ungroup() %>% 
  filter(avg_prop > 0.005) %>% 
  group_by(country, age, c_jh) %>% 
  filter(!any(dts == 0)) %>% 
  ungroup() %>% 
  select(-prop, -avg_prop) 

# all cause mortality by age
db_young <- 
  db5 %>% 
  group_by(country, age, year, age_type, g_jh) %>% 
  summarise(dts_all = sum(dts)) %>% 
  ungroup()

# grouping no selected causes in residual category
resid <- 
  dts_causes %>% 
  group_by(country, age, year, age_type, g_jh) %>% 
  summarise(dts_cod = sum(dts)) %>% 
  ungroup() %>% 
  left_join(db_young) %>% 
  mutate(dts = dts_all - dts_cod) %>% 
  mutate(c_jh = case_when(g_jh == "gr1" ~ "c120_residual_gr1",
                          g_jh == "gr2" ~ "c150_residual_gr2",
                          g_jh == "gr3" ~ "c160",
                          g_jh == "gr4" ~ "c900")) %>% 
  select(country, age, age_type, year, g_jh, c_jh, dts) %>% 
  group_by(country, age, age_type, year, g_jh, c_jh) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

# adding residuals
dts_causes2 <- 
  dts_causes %>% 
  bind_rows(resid,
            dts_covid) %>%
  group_by(country, age, age_type, year, g_jh, c_jh) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  arrange(country, age, year, g_jh, c_jh) %>% 
  group_by(country, age, c_jh) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(w = ifelse(year <= 2019, 1, 0))

# contribution of selected causes
conts <- 
  dts_causes2 %>% 
  group_by(country, age, c_jh) %>% 
  mutate(dts_all_c_jh = sum(dts)) %>% 
  ungroup() %>% 
  group_by(country, age) %>% 
  mutate(cont = dts_all_c_jh / sum(dts)) %>% 
  ungroup() %>% 
  select(country, age, c_jh, cont) %>% 
  unique() %>% 
  arrange(country, age, c_jh)




# ~~~~~~~~~~~~~~~~~~
# exposure data ====
# ~~~~~~~~~~~~~~~~~~
exp_yng <- read_rds("data_inter/annual_exposure_5y_groups.rds")
exp_inf <- read_rds("data_inter/annual_exposure_infant_child.rds")

unique(exp_yng$Country)
unique(exp_inf$Country)

cts_yng <- 
  yng %>% 
  pull(country) %>% 
  unique()

cts_inf <- 
  inf %>% 
  pull(country) %>% 
  unique()

exp_yng2 <- 
  exp_yng %>% 
  filter(Sex == "t",
         Age != 0) %>% 
  select(country = Country,
         age_exp = Age,
         year = Year,
         exposure = Population)

exp_inf2 <- 
  exp_inf %>% 
  filter(Sex == "t") %>% 
  select(country = Country,
         age_exp = Age,
         year = Year,
         exposure = Population)

unique(exp_inf2$age_exp)
unique(exp_yng2$age_exp)

exps <- 
  bind_rows(exp_inf2, exp_yng2)


dts_causes2$age %>% unique()

dts_causes3 <- 
  dts_causes2 %>% 
  mutate(
    age_exp = case_when(age %in% c("0", "neo", "post_neo") ~ "0",
                        TRUE ~ age),
    age_exp = age_exp %>% as.double()) %>%
  left_join(exps) %>% 
  rename(cod = c_jh)

dts_causes3$cod %>% unique() %>% sort()

test <- 
  dts_causes3 %>% 
  filter(is.na(exposure))

# ===================
write_rds(dts_causes3, "Output/deaths_by_cause_jh_db_and_exposures.rds")



# 
# db <- temp2
# 
# pred_this <- function(db){
#   Year <- db %>% drop_na() %>% pull(Year)
#   Deaths <- db %>% drop_na() %>% pull(Deaths)
#   new <- tibble(Year = min(Year):2020)
#   res <- predict(lm(Deaths ~ Year), new)
#   out <- tibble(Year = min(Year):2020, 
#                 Deaths = as.numeric(res))
#   return(out)
# }
# 
# pred_this <- function(db){
#   Year <- db %>% drop_na() %>% pull(Year)
#   Deaths <- (db %>% drop_na() %>% pull(Deaths) + 1) %>% log()
#   new <- tibble(Year = min(Year):2020)
#   res <- (predict(lm(Deaths ~ Year), new) %>% exp()) - 1
#   out <- tibble(Year = min(Year):2020, 
#                 Deaths = as.numeric(res))
#   return(out)
# }
# 
# 
# 
# 
# ct <- "Austria"
# a <- 0
# c <- "c99"
# 
# cts <- unique(db20_3$Country)
# as <- unique(db20_3$Age)
# out <- list()
# 
# for(ct in cts){
#   temp <- 
#     db20_3 %>% 
#     filter(Country == ct)
#   
#   for(a in as){
#     
#     temp0 <- 
#       temp %>% 
#       filter(Age == a)
#     cs <- unique(temp0$Cause)
#     
#     for(c in cs){
#       
#       temp1 <-  
#         temp0 %>% 
#         filter(Cause == c)
#       
#       temp2 <- 
#         temp1 %>% 
#         filter(Year < 2020)
#       
#       # temp2 <- 
#       #   spline_this(temp1, 1e-5) %>% 
#       #   mutate(type = "spline")
#       
#       temp3 <- 
#         pred_this(temp2) %>% 
#         mutate(type = "linear")
#       
#       iter <- paste0(ct, "_", a, "_", c)
#       cat(paste0(iter, "\n"))
#       out[[iter]] <- 
#         temp1 %>% 
#         select(Year, Deaths) %>% 
#         mutate(type = "observed") %>% 
#         bind_rows(temp3) %>% 
#         mutate(Country = ct,
#                Age = a,
#                Cause = c)
#     }
#   }
# }
# 
# db20_4 <- 
#   out %>% 
#   bind_rows() %>% 
#   mutate(Deaths = ifelse(Deaths >= 0, Deaths %>% round(), 0)) %>% 
#   group_by(Country, Year, Age, type) %>% 
#   mutate(prop_year = Deaths / sum(Deaths)) %>% 
#   ungroup()
# 
# # ~~~~~~~~~~~~~~~~
# unique(db20_4$Age)  
# 
# diffs_2020 <- 
#   db20_4 %>% 
#   select(Country, Year, Age, Cause, Deaths, type) %>% 
#   filter(Year == 2020) %>% 
#   spread(type, Deaths) %>% 
#   mutate(diff = observed - linear)
# 
# # contribution to negative changes in mortality (reduction)
# neg_conts_2020 <- 
#   diffs_2020 %>% 
#   select(Country, Age, Cause, diff) %>% 
#   filter(diff < 0) %>% 
#   group_by(Country, Age) %>% 
#   mutate(cont = diff/sum(diff)) %>% 
#   ungroup()
# 
# # contribution to positive changes in mortality (increase)
# pos_conts_2020 <- 
#   diffs_2020 %>% 
#   select(Country, Age, Cause, diff) %>% 
#   filter(diff > 0) %>% 
#   group_by(Country, Age) %>% 
#   mutate(cont = diff/sum(diff)) %>% 
#   ungroup()
# 
# # selecting leading causes contributing to at least 60%
# neg_conts_2020_filter <- 
#   neg_conts_2020 %>% 
#   arrange(Country, Age, -cont) %>% 
#   group_by(Country, Age) %>% 
#   mutate(cum_cont = cumsum(cont),
#          id = 1:n(),
#          rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
#   ungroup() %>% 
#   mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
#   group_by(Country, Age, Cause) %>% 
#   summarise(diff = sum(diff)) %>% 
#   ungroup() %>% 
#   group_by(Country, Age) %>% 
#   mutate(cont = diff / sum(diff)) %>% 
#   ungroup() %>% 
#   mutate(cont_type = "negative")
# 
# pos_conts_2020_filter <- 
#   pos_conts_2020 %>% 
#   arrange(Country, Age, -cont) %>% 
#   group_by(Country, Age) %>% 
#   mutate(cum_cont = cumsum(cont),
#          id = 1:n(),
#          rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
#   ungroup() %>% 
#   mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
#   group_by(Country, Age, Cause) %>% 
#   summarise(diff = sum(diff)) %>% 
#   ungroup() %>% 
#   group_by(Country, Age) %>% 
#   mutate(cont = diff / sum(diff)) %>% 
#   ungroup() %>% 
#   mutate(cont_type = "positive")
# 
# pos_conts_2020 %>% 
#   filter(Country == "Austria") %>% 
#   ggplot()+
#   geom_bar(aes(x = factor(Age), y = cont, fill = fct_reorder(Cause, -cont)), 
#            stat = "identity", 
#            position= "fill")+
#   coord_cartesian(expand = 0)
# 
# # ggsave(paste0("Figures/causes/cont_cause_brazil_young.png"), dpi = 600)
# 
# 
# 
# 
# conts_2020_filter <- 
#   bind_rows(pos_conts_2020_filter,
#             neg_conts_2020_filter) %>%
#   mutate(cont = ifelse(cont_type == "positive", cont, -cont))
# 
# 
# write_rds(conts_2020_filt, "Output/causes_changes_2020_who_mort_db.rds")
# 
# 
# causes_cods <- read_xlsx("Data/becker_codes.xlsx") 
# 
# causes_cods2 <- 
#   causes_cods %>% 
#   mutate(Cause = paste0("c", id)) %>% 
#   select(Cause, causes, codes)
# 
# 
# db_cs <- 
#   conts_2020_filter %>% 
#   left_join(causes_cods2) %>% 
#   mutate(Cause_des = case_when(Cause == 'c5' ~ 'Meningitis', 
#                                Cause == 'c18' ~ 'Cancer uterus', 
#                                Cause == 'c24' ~ 'Cancer lymphoid', 
#                                Cause == 'c28' ~ 'Dehydratation',
#                                Cause == 'c36' ~ 'Cardiorespiratory', 
#                                Cause == 'c46' ~ 'Influenza and Pneumonia', 
#                                Cause == 'c48' ~ 'Pulmonary oedema', 
#                                Cause == 'c49' ~ 'Respiratory failure', 
#                                Cause == 'c53' ~ 'Urinary syst', 
#                                Cause == 'c55' ~ 'Perinatal', 
#                                Cause == 'c56' ~ 'Congenital', 
#                                Cause == 'c57' ~ 'Traffic accidents', 
#                                Cause == 'c6' ~ 'Septicaemia', 
#                                Cause == 'c60' ~ 'Accidental drowning', 
#                                Cause == 'c63' ~ 'Suicide', 
#                                Cause == 'c64' ~ 'Homicide', 
#                                Cause == 'c72' ~ 'COVID-19', 
#                                Cause == 'c88' ~ 'Remainder', 
#                                Cause == 'c99' ~ 'Ill-defined'))
# 
# ext <- c("c57", "c60", "c63", "c64")
# res_inf <- c("c6", "c46", "c48", "c49", "c71")
# 
# css <- 
#   db_cs %>% 
#   select(Cause, causes) %>% 
#   group_by(Cause) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   arrange(-n)
# 
# write.excel(css)
# 
# cols <- 
#   c(
#     # external
#     "Traffic accidents" = "#660708", 
#     "Accidental drowning" = "#a4161a", 
#     "Suicide" = "#ba181b", 
#     "Homicide" = "#e5383b",
#     # respiratory and infectious
#     "Meningitis" = "#007f5f",
#     "Septicaemia" = "#2b9348", 
#     "Influenza and Pneumonia" = "#80b918", 
#     "Pulmonary oedema" = "#bfd200", 
#     "Respiratory failure" = "#eeef20", 
#     # "Cardiorespiratory" = "#eeef20",
#     # congenital / perinatal
#     "Perinatal" = "#7b2cbf",
#     "Congenital" = "#c77dff",
#     # others
#     # cancer
#     "Cancer lymphoid" = "#01497c",
#     # 'Cancer uterus' = "#2a6f97",
#     # 'Urinary syst' = "#468faf",
#     # 'Dehydratation' = "#89c2d9",
#     # rest
#     "Ill-defined" = "#936639",
#     "Remainder" = "grey"
#   )
# 
# 
# levs <- 
#   c(
#     # external
#     "Traffic accidents", 
#     "Accidental drowning", 
#     "Suicide", 
#     "Homicide",
#     # respiratory and infectious
#     "Meningitis",
#     "Septicaemia", 
#     "Influenza and Pneumonia", 
#     "Pulmonary oedema", 
#     "Respiratory failure", 
#     "Cardiorespiratory",
#     # congenital / perinatal
#     "Perinatal",
#     "Congenital",
#     # others
#     # cancer
#     "Cancer lymphoid",
#     'Cancer uterus',
#     'Urinary syst',
#     'Dehydratation',
#     # rest
#     "Ill-defined",
#     "Remainder"
#   )
# 
# # db_cs %>% 
# #   mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
# #   ggplot()+
# #   geom_bar(aes(x = factor(Age), y = cont, fill = Cause_des), 
# #            stat = "identity", 
# #            position="fill")+
# #   coord_cartesian(expand = 0)+
# #   scale_fill_manual(values = cols)+
# #   facet_grid(~ Country)+
# #   labs(fill = "Cause",
# #        x = "Age group",
# #        y = "Neg. contribution")+
# #   theme(legend.key.size = unit(0.2, "cm"))
# # 
# # ggsave("Figures/causes/composition_change_who.png", width = 8, height = 4.09)
# 
# a <- 20
# 
# db_cs %>% 
#   filter(Age == a) %>% 
#   mutate(Country = str_replace(Country, "United Kingdom, ", ""),
#          Country = ifelse(Country == "Czech Republic", "Czechia", Country)) %>% 
#   mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
#   ggplot()+
#   geom_bar(aes(x = cont, y = factor(Country), fill = Cause_des), 
#            stat = "identity", 
#            position="fill")+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   scale_fill_manual(values = cols)+
#   coord_cartesian(expand = 0)+
#   labs(fill = "Cause",
#        x = "Change contribution",
#        title = paste0("Age ", a))+
#   theme(legend.key.size = unit(0.2, "cm"),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/cod/who_cause_contribution_age_", a, ".png"),
#        dpi = 600)
# 
# db_cs %>% 
#   # filter(Age == a) %>% 
#   mutate(Country = str_replace(Country, "United Kingdom, ", ""),
#          Country = ifelse(Country == "Czech Republic", "Czechia", Country)) %>% 
#   mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
#   ggplot()+
#   geom_bar(aes(x = cont, y = factor(Country), fill = Cause_des), 
#            stat = "identity", 
#            position="fill")+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   scale_fill_manual(values = cols)+
#   coord_cartesian(expand = 0)+
#   facet_grid(~Age)+
#   labs(fill = "Cause",
#        x = "Change contribution")+
#   theme(legend.key.size = unit(0.2, "cm"),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(size = 7))
# 
# ggsave(paste0("Figures/cod/who_cause_contribution_age_all.png"),
#        dpi = 600)
