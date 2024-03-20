library(readxl)
rm (list = ls())
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
  pull(name) %>% 
  unique()

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
unique(db2$age)


db3 <- 
  db2 %>% 
  mutate(cause = str_sub(cause, 1, 3)) %>% 
  filter(cause != "AAA") %>% 
  mutate(code_sub_chp = case_when(
           cause %in% paste0('F', sprintf('%02d', 00:09)) ~ 'A00-A09',
           cause %in% paste0('A', sprintf('%02d', 15:19)) ~ 'A15-A19',
           cause %in% paste0('A', sprintf('%02d', 20:28)) ~ 'A20-A28',
           cause %in% paste0('A', sprintf('%02d', 30:49)) ~ 'A30-A49',
           cause %in% paste0('A', sprintf('%02d', 50:64)) ~ 'A50-A64',
           cause %in% paste0('A', sprintf('%02d', 65:69)) ~ 'A65-A69',
           cause %in% paste0('A', sprintf('%02d', 70:74)) ~ 'A70-A74',
           cause %in% paste0('A', sprintf('%02d', 75:79)) ~ 'A75-A79',
           cause %in% paste0('A', sprintf('%02d', 80:89)) ~ 'A80-A89',
           cause %in% paste0('A', sprintf('%02d', 90:99)) ~ 'A90-A99',
           cause %in% paste0('B', sprintf('%02d', 00:09)) ~ 'B00-B09',
           cause %in% paste0('B', sprintf('%02d', 15:19)) ~ 'B15-B19',
           cause %in% paste0('B', sprintf('%02d', 20:24)) ~ 'B20-B24',
           cause %in% paste0('B', sprintf('%02d', 25:34)) ~ 'B25-B34',
           cause %in% paste0('B', sprintf('%02d', 35:49)) ~ 'B35-B49',
           cause %in% paste0('B', sprintf('%02d', 50:64)) ~ 'B50-B64',
           cause %in% paste0('B', sprintf('%02d', 65:83)) ~ 'B65-B83',
           cause %in% paste0('B', sprintf('%02d', 85:89)) ~ 'B85-B89',
           cause %in% paste0('B', sprintf('%02d', 90:94)) ~ 'B90-B94',
           cause %in% paste0('B', sprintf('%02d', 95:98)) ~ 'B95-B98',
           cause %in% paste0('B', sprintf('%02d', 99:99)) ~ 'B99-B99',
           cause %in% paste0('C', sprintf('%02d', 00:97)) ~ 'C00-C97',
           cause %in% paste0('D', sprintf('%02d', 00:09)) ~ 'D00-D09',
           cause %in% paste0('D', sprintf('%02d', 10:36)) ~ 'D10-D36',
           cause %in% paste0('D', sprintf('%02d', 37:48)) ~ 'D37-D48',
           cause %in% paste0('D', sprintf('%02d', 50:53)) ~ 'D50-D53',
           cause %in% paste0('D', sprintf('%02d', 55:59)) ~ 'D55-D59',
           cause %in% paste0('D', sprintf('%02d', 60:64)) ~ 'D60-D64',
           cause %in% paste0('D', sprintf('%02d', 65:69)) ~ 'D65-D69',
           cause %in% paste0('D', sprintf('%02d', 70:76)) ~ 'D70-D76',
           cause %in% paste0('D', sprintf('%02d', 80:89)) ~ 'D80-D89',
           cause %in% paste0('E', sprintf('%02d', 00:07)) ~ 'E00-E07',
           cause %in% paste0('E', sprintf('%02d', 10:14)) ~ 'E10-E14',
           cause %in% paste0('E', sprintf('%02d', 15:16)) ~ 'E15-E16',
           cause %in% paste0('E', sprintf('%02d', 20:34)) ~ 'E20-E34',
           cause %in% paste0('E', sprintf('%02d', 40:46)) ~ 'E40-E46',
           cause %in% paste0('E', sprintf('%02d', 50:64)) ~ 'E50-E64',
           cause %in% paste0('E', sprintf('%02d', 65:68)) ~ 'E65-E68',
           cause %in% paste0('E', sprintf('%02d', 70:88)) ~ 'E70-E88',
           cause %in% paste0('F', sprintf('%02d', 01:09)) ~ 'F01-F09',
           cause %in% paste0('F', sprintf('%02d', 10:19)) ~ 'F10-F19',
           cause %in% paste0('F', sprintf('%02d', 20:29)) ~ 'F20-F29',
           cause %in% paste0('F', sprintf('%02d', 30:39)) ~ 'F30-F39',
           cause %in% paste0('F', sprintf('%02d', 40:48)) ~ 'F40-F48',
           cause %in% paste0('F', sprintf('%02d', 50:59)) ~ 'F50-F59',
           cause %in% paste0('F', sprintf('%02d', 60:69)) ~ 'F60-F69',
           cause %in% paste0('F', sprintf('%02d', 70:79)) ~ 'F70-F79',
           cause %in% paste0('F', sprintf('%02d', 80:89)) ~ 'F80-F89',
           cause %in% paste0('F', sprintf('%02d', 90:98)) ~ 'F90-F98',
           cause %in% paste0('F', sprintf('%02d', 99:99)) ~ 'F99-F99',
           cause %in% paste0('G', sprintf('%02d', 00:09)) ~ 'G00-G09',
           cause %in% paste0('G', sprintf('%02d', 10:14)) ~ 'G10-G14',
           cause %in% paste0('G', sprintf('%02d', 20:25)) ~ 'G20-G25',
           cause %in% paste0('G', sprintf('%02d', 30:31)) ~ 'G30-G31',
           cause %in% paste0('G', sprintf('%02d', 35:37)) ~ 'G35-G37',
           cause %in% paste0('G', sprintf('%02d', 40:47)) ~ 'G40-G47',
           cause %in% paste0('G', sprintf('%02d', 50:58)) ~ 'G50-G58',
           cause %in% paste0('G', sprintf('%02d', 60:64)) ~ 'G60-G64',
           cause %in% paste0('G', sprintf('%02d', 70:72)) ~ 'G70-G72',
           cause %in% paste0('G', sprintf('%02d', 80:83)) ~ 'G80-G83',
           cause %in% paste0('G', sprintf('%02d', 90:98)) ~ 'G90-G98',
           cause %in% paste0('H', sprintf('%02d', 00:93)) ~ 'H00-H93',
           cause %in% paste0('I', sprintf('%02d', 00:02)) ~ 'I00-I02',
           cause %in% paste0('I', sprintf('%02d', 05:09)) ~ 'I05-I09',
           cause %in% paste0('I', sprintf('%02d', 10:15)) ~ 'I10-I15',
           cause %in% paste0('I', sprintf('%02d', 20:25)) ~ 'I20-I25',
           cause %in% paste0('I', sprintf('%02d', 26:28)) ~ 'I26-I28',
           cause %in% paste0('I', sprintf('%02d', 30:51)) ~ 'I30-I51',
           cause %in% paste0('I', sprintf('%02d', 60:69)) ~ 'I60-I69',
           cause %in% paste0('I', sprintf('%02d', 70:78)) ~ 'I70-I78',
           cause %in% paste0('I', sprintf('%02d', 80:89)) ~ 'I80-I89',
           cause %in% paste0('I', sprintf('%02d', 95:99)) ~ 'I95-I99',
           cause %in% paste0('J', sprintf('%02d', 00:06)) ~ 'J00-J06',
           cause %in% paste0('J', sprintf('%02d', 09:18)) ~ 'J09-J18',
           cause %in% paste0('J', sprintf('%02d', 20:22)) ~ 'J20-J22',
           cause %in% paste0('J', sprintf('%02d', 30:39)) ~ 'J30-J39',
           cause %in% paste0('J', sprintf('%02d', 40:47)) ~ 'J40-J47',
           cause %in% paste0('J', sprintf('%02d', 60:70)) ~ 'J60-J70',
           cause %in% paste0('J', sprintf('%02d', 80:84)) ~ 'J80-J84',
           cause %in% paste0('J', sprintf('%02d', 85:86)) ~ 'J85-J86',
           cause %in% paste0('J', sprintf('%02d', 90:94)) ~ 'J90-J94',
           cause %in% paste0('J', sprintf('%02d', 96:98)) ~ 'J96-J98',
           cause %in% paste0('K', sprintf('%02d', 00:14)) ~ 'K00-K14',
           cause %in% paste0('K', sprintf('%02d', 20:31)) ~ 'K20-K31',
           cause %in% paste0('K', sprintf('%02d', 35:38)) ~ 'K35-K38',
           cause %in% paste0('K', sprintf('%02d', 40:46)) ~ 'K40-K46',
           cause %in% paste0('K', sprintf('%02d', 50:52)) ~ 'K50-K52',
           cause %in% paste0('K', sprintf('%02d', 55:64)) ~ 'K55-K64',
           cause %in% paste0('K', sprintf('%02d', 65:66)) ~ 'K65-K66',
           cause %in% paste0('K', sprintf('%02d', 70:76)) ~ 'K70-K76',
           cause %in% paste0('K', sprintf('%02d', 80:86)) ~ 'K80-K86',
           cause %in% paste0('K', sprintf('%02d', 90:92)) ~ 'K90-K92',
           cause %in% paste0('L', sprintf('%02d', 00:08)) ~ 'L00-L08',
           cause %in% paste0('L', sprintf('%02d', 10:13)) ~ 'L10-L13',
           cause %in% paste0('L', sprintf('%02d', 20:30)) ~ 'L20-L30',
           cause %in% paste0('L', sprintf('%02d', 40:44)) ~ 'L40-L44',
           cause %in% paste0('L', sprintf('%02d', 50:53)) ~ 'L50-L53',
           cause %in% paste0('L', sprintf('%02d', 55:59)) ~ 'L55-L59',
           cause %in% paste0('L', sprintf('%02d', 60:75)) ~ 'L60-L75',
           cause %in% paste0('L', sprintf('%02d', 80:98)) ~ 'L80-L98',
           cause %in% paste0('M', sprintf('%02d', 00:25)) ~ 'M00-M25',
           cause %in% paste0('M', sprintf('%02d', 30:35)) ~ 'M30-M35',
           cause %in% paste0('M', sprintf('%02d', 40:54)) ~ 'M40-M54',
           cause %in% paste0('M', sprintf('%02d', 60:79)) ~ 'M60-M79',
           cause %in% paste0('M', sprintf('%02d', 80:94)) ~ 'M80-M94',
           cause %in% paste0('M', sprintf('%02d', 95:99)) ~ 'M95-M99',
           cause %in% paste0('N', sprintf('%02d', 00:07)) ~ 'N00-N07',
           cause %in% paste0('N', sprintf('%02d', 10:15)) ~ 'N10-N15',
           cause %in% paste0('N', sprintf('%02d', 17:19)) ~ 'N17-N19',
           cause %in% paste0('N', sprintf('%02d', 20:23)) ~ 'N20-N23',
           cause %in% paste0('N', sprintf('%02d', 25:28)) ~ 'N25-N28',
           cause %in% paste0('N', sprintf('%02d', 30:39)) ~ 'N30-N39',
           cause %in% paste0('N', sprintf('%02d', 40:50)) ~ 'N40-N50',
           cause %in% paste0('N', sprintf('%02d', 60:64)) ~ 'N60-N64',
           cause %in% paste0('N', sprintf('%02d', 70:76)) ~ 'N70-N76',
           cause %in% paste0('N', sprintf('%02d', 80:98)) ~ 'N80-N98',
           cause %in% paste0('O', sprintf('%02d', 00:07)) ~ 'O00-O07',
           cause %in% paste0('O', sprintf('%02d', 10:16)) ~ 'O10-O16',
           cause %in% paste0('O', sprintf('%02d', 20:29)) ~ 'O20-O29',
           cause %in% paste0('O', sprintf('%02d', 30:48)) ~ 'O30-O48',
           cause %in% paste0('O', sprintf('%02d', 60:75)) ~ 'O60-O75',
           cause %in% paste0('O', sprintf('%02d', 85:92)) ~ 'O85-O92',
           cause %in% paste0('O', sprintf('%02d', 95:99)) ~ 'O95-O99',
           cause %in% paste0('P', sprintf('%02d', 00:04)) ~ 'P00-P04',
           cause %in% paste0('P', sprintf('%02d', 05:08)) ~ 'P05-P08',
           cause %in% paste0('P', sprintf('%02d', 10:15)) ~ 'P10-P15',
           cause %in% paste0('P', sprintf('%02d', 20:29)) ~ 'P20-P29',
           cause %in% paste0('P', sprintf('%02d', 35:39)) ~ 'P35-P39',
           cause %in% paste0('P', sprintf('%02d', 50:61)) ~ 'P50-P61',
           cause %in% paste0('P', sprintf('%02d', 70:74)) ~ 'P70-P74',
           cause %in% paste0('P', sprintf('%02d', 76:78)) ~ 'P76-P78',
           cause %in% paste0('P', sprintf('%02d', 80:83)) ~ 'P80-P83',
           cause %in% paste0('P', sprintf('%02d', 90:96)) ~ 'P90-P96',
           cause %in% paste0('Q', sprintf('%02d', 00:07)) ~ 'Q00-Q07',
           cause %in% paste0('Q', sprintf('%02d', 10:18)) ~ 'Q10-Q18',
           cause %in% paste0('Q', sprintf('%02d', 20:28)) ~ 'Q20-Q28',
           cause %in% paste0('Q', sprintf('%02d', 30:34)) ~ 'Q30-Q34',
           cause %in% paste0('Q', sprintf('%02d', 35:37)) ~ 'Q35-Q37',
           cause %in% paste0('Q', sprintf('%02d', 38:45)) ~ 'Q38-Q45',
           cause %in% paste0('Q', sprintf('%02d', 50:56)) ~ 'Q50-Q56',
           cause %in% paste0('Q', sprintf('%02d', 60:64)) ~ 'Q60-Q64',
           cause %in% paste0('Q', sprintf('%02d', 65:79)) ~ 'Q65-Q79',
           cause %in% paste0('Q', sprintf('%02d', 80:89)) ~ 'Q80-Q89',
           cause %in% paste0('Q', sprintf('%02d', 90:99)) ~ 'Q90-Q99',
           cause %in% paste0('R', sprintf('%02d', 00:09)) ~ 'R00-R09',
           cause %in% paste0('R', sprintf('%02d', 10:19)) ~ 'R10-R19',
           cause %in% paste0('R', sprintf('%02d', 20:23)) ~ 'R20-R23',
           cause %in% paste0('R', sprintf('%02d', 25:29)) ~ 'R25-R29',
           cause %in% paste0('R', sprintf('%02d', 30:39)) ~ 'R30-R39',
           cause %in% paste0('R', sprintf('%02d', 40:46)) ~ 'R40-R46',
           cause %in% paste0('R', sprintf('%02d', 47:49)) ~ 'R47-R49',
           cause %in% paste0('R', sprintf('%02d', 50:68)) ~ 'R50-R68',
           cause %in% paste0('R', sprintf('%02d', 70:79)) ~ 'R70-R79',
           cause %in% paste0('R', sprintf('%02d', 80:89)) ~ 'R80-R89',
           cause %in% paste0('R', sprintf('%02d', 90:94)) ~ 'R90-R94',
           cause %in% paste0('R', sprintf('%02d', 95:99)) ~ 'R95-R99',
           cause %in% paste0('U', sprintf('%02d', 00:06)) ~ 'U00-U49',
           cause %in% paste0('U', sprintf('%02d', 07)) ~ 'U071',
           cause %in% paste0('U', sprintf('%02d', 08:49)) ~ 'U00-U49',
           cause %in% paste0('V', sprintf('%02d', 01:99)) ~ 'V01-V99',
           cause %in% paste0('W', sprintf('%02d', 00:99)) ~ 'W00-X59',
           cause %in% paste0('X', sprintf('%02d', 00:59)) ~ 'W00-X59',
           cause %in% paste0('X', sprintf('%02d', 60:84)) ~ 'X60-X84',
           cause %in% paste0('X', sprintf('%02d', 85:99)) ~ 'X85-Y09',
           cause %in% paste0('Y', sprintf('%02d', 00:09)) ~ 'X85-Y09',
           cause %in% paste0('Y', sprintf('%02d', 10:34)) ~ 'Y10-Y34',
           cause %in% paste0('Y', sprintf('%02d', 35:36)) ~ 'Y35-Y36',
           cause %in% paste0('Y', sprintf('%02d', 40:84)) ~ 'Y40-Y84',
           cause %in% paste0('Y', sprintf('%02d', 85:89)) ~ 'Y85-Y89',
           TRUE ~ "other")) %>% 
  group_by(country, year, age, age_type, code_sub_chp) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

# others <-
#   db3 %>%
#   filter(code_sub_chp == "other") %>%
#   select(cause, code_sub_chp) %>%
#   group_by(cause, code_sub_chp) %>%
#   summarise(n = n()) %>%
#   ungroup()

# excluding deaths recorded as COVID-19
db4 <- 
  db3 %>% 
  filter(!code_sub_chp %in% 'U071')


# complete all observations with zeros
inf <- 
  db4 %>% 
  filter(age_type == "infant") %>% 
  complete(country, year, age_type, age, code_sub_chp, fill = list(dts = 0)) 

unique(inf$age)

yng <- 
  db4 %>% 
  filter(age_type == "young") %>% 
  complete(country, year, age_type, age, code_sub_chp, fill = list(dts = 0))

unique(yng$age)

db5 <- 
  bind_rows(inf, yng) %>% 
  group_by(country, age_type, age, year, code_sub_chp)


# deaths recorded as covid ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_covid <- 
  db3 %>% 
  filter(code_sub_chp %in% 'U071')

# grouping causes that do not make at least 5% on average de contribution to 
# all cause mortality
unique(db5$age)

dts_causes <- 
  db5 %>% 
  group_by(country, age, year) %>% 
  mutate(prop = dts / sum(dts)) %>% 
  group_by(country, age, code_sub_chp) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  ungroup() %>% 
  filter(avg_prop > 0.001) %>% 
  group_by(country, age, code_sub_chp) %>% 
  filter(!any(dts == 0)) %>%
  ungroup() %>% 
  select(-prop, -avg_prop) 

# all cause mortality by age
db_young <- 
  db5 %>% 
  group_by(country, age, year, age_type) %>% 
  summarise(dts_all = sum(dts)) %>% 
  ungroup()

# grouping no selected causes in residual category
resid <- 
  dts_causes %>% 
  group_by(country, age, year, age_type) %>% 
  summarise(dts_cod = sum(dts)) %>% 
  left_join(db_young) %>% 
  mutate(dts = dts_all - dts_cod,
         code_sub_chp = "Z_residual") %>% 
  select(country, age, age_type, year, code_sub_chp, dts)

# adding residuals
dts_causes2 <- 
  dts_causes %>% 
  bind_rows(resid,
            dts_covid) %>% 
  arrange(country, age, year, code_sub_chp) %>% 
  # left_join(pop) %>% 
  group_by(country, age, code_sub_chp) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(w = ifelse(year <= 2019, 1, 0))

# contribution of selected causes
conts <- 
  dts_causes2 %>% 
  group_by(country, age, code_sub_chp) %>% 
  mutate(dts_all_sub_chp = sum(dts)) %>% 
  ungroup() %>% 
  group_by(country, age) %>% 
  mutate(cont = dts_all_sub_chp / sum(dts)) %>% 
  ungroup() %>% 
  select(country, age, code_sub_chp, cont) %>% 
  unique() %>% 
  arrange(country, age, code_sub_chp)




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
dts_causes3$cod %>% unique() %>% sort()

dts_causes3 <- 
  dts_causes2 %>% 
  mutate(
    age_exp = case_when(age %in% c("0", "neo", "post_neo") ~ "0",
                        TRUE ~ age),
    age_exp = age_exp %>% as.double()) %>%
  left_join(exps) %>% 
  rename(cod = code_sub_chp)

test <- 
  dts_causes3 %>% 
  filter(is.na(exposure))
test 
# ===================
write_rds(dts_causes3, "data_inter/deaths_by_cause_who_db_and_exposures.rds")

