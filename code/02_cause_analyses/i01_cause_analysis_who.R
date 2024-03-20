rm (list = ls())
source("Code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~
# processing data for 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~
icd_all <-
  read_rds("data_inter/who_raw.rds") %>% 
  select(-Country) %>% 
  rename(country = name) %>% 
  mutate(country = case_when(country == "United States of America" ~ "USA",
                             country == "Czech Republic" ~ "Czechia",
                             TRUE ~ country))

unique(icd_all$country) %>% sort

# # not yet data for 2022
test22 <-
  icd_all %>%
  mutate(Year = Year %>% as.double()) %>%
  group_by(country) %>%
  filter(max(Year) >= 2022 &  Frmat != "09") %>%
  ungroup()

# # summary of 2020 and 2021
who_has_20 <-
  icd_all %>%
  mutate(Year = Year %>% as.double()) %>%
  group_by(country) %>%
  filter(max(Year) >= 2020 &  Frmat != "09") %>%
  ungroup() %>% 
  filter(Year >= 2020) %>% 
  select(country, Year) %>% unique() %>% 
  mutate(avail = "x") %>% 
  spread(Year, avail)

pop <- read_rds("data_inter/wpp_populations_ages_0_24_v2022.rds")
unique(pop$age)
unique(pop$country)

pop2 <- 
  pop %>% 
  filter(year == 2020,
         sex == "t") %>% 
  group_by(country, code) %>% 
  summarise(pop = sum(pop))

cts <- 
  who_has_20 %>% 
  left_join(pop2)

cts2 <- 
  cts %>% 
  filter(pop > 19e6) %>% 
  pull(country)

dt <- 
  icd_all %>% 
  filter(country %in% cts2)


# By year, format of age groups, and icd10 classification
dt2 <- 
  dt %>%
  # 2014 , format 0 and ICD list 104 have the most years (115)
  filter(Year == "2014", Frmat == "00", List == "104") %>%
  # keeping only relevant variables
  dplyr::select(-c(Admin1,SubDiv,List,Frmat,IM_Frmat,IM_Deaths1,IM_Deaths2,IM_Deaths3,IM_Deaths4))


test <- 
  dt %>%
  # 2014 , format 0 and ICD list 104 have the most years (115)
  filter(Year == "2014", Frmat %in% c("00", "01", "02"), List == "104") %>%
  # keeping only relevant variables
  dplyr::select(-c(Admin1,SubDiv,List,Frmat,IM_Frmat,IM_Deaths1,IM_Deaths2,IM_Deaths3,IM_Deaths4))

unique(test$country)
unique(dt2$country)

# including age formats 01 and 02 reach 90 countries
# including age formats 00 reach 85 countries


#=================================================================================================#
# Grouping causes
#=================================================================================================#

dt3 <- 
  dt2 %>% 
  gather(Deaths2:Deaths26, key = age_cat, value = dts) %>% 
  rename(cause = Cause,
         sex = Sex,
         year = Year) %>% 
  mutate(dts = dts %>% as.double(),
         dts = ifelse(is.na(dts), 0, dts),
         age2 = str_sub(age_cat, 7, 8) %>% as.integer(),
         age = case_when(age2 == 2 ~ 0,
                         age2 >= 3 & age2 <= 6 ~ 1,
                         age2 >= 7 & age2 <= 25 ~ (age2 - 6) * 5,
                         age2 == 26 ~ NA_real_)) %>%
  filter(cause != "AAA", # exclude all-cause (AAA) mortality combined
         !is.na(age), # exclude unknown age
         sex %in% c(1, 2)) %>% 
  mutate(cause = str_sub(cause, 1, 3),
         ucod = case_when(
           cause %in% paste0('A', sprintf('%02d', 00:09)) ~ 'A00-A09',
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
           TRUE ~ "other"))  %>% 
  group_by(country, year, sex, age, ucod) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()  

unique(dt3$country)
unique(dt3$ucod)
unique(dt3$sex)


dt4 <- 
  dt3 %>% 
  mutate(let_cause = str_sub(ucod, 1, 1),
         num2_cause = str_sub(ucod, 2, 3) %>% as.integer()) %>% 
  mutate(cause = case_when(
    let_cause == "A" | let_cause == "B" ~ "infectious", 
    let_cause == "C" | (let_cause == "D" & num2_cause <= 49) ~ "cancer",
    let_cause == "D" & num2_cause >= 50 ~ "blood",
    let_cause == "E" ~ "endocrine",
    let_cause == "F" ~ "mental",
    let_cause == "G" ~ "nervous",
    let_cause == "H" & num2_cause <= 59 ~ "eye",
    let_cause == "H" & num2_cause >= 60 ~ "ear",
    let_cause == "I" ~ "circulatory",
    let_cause == "J" ~ "respiratory",
    let_cause == "K" ~ "digestive",
    let_cause == "L" ~ "skin",
    let_cause == "M" ~ "muscoloskeletal",
    let_cause == "N" ~ "genitourinary",
    let_cause == "O" ~ "pregnancy",
    let_cause == "P" ~ "perinatal",
    let_cause == "Q" ~ "congenital",
    let_cause == "S" | let_cause == "T" |
      let_cause == "V" | let_cause == "W" |
      let_cause == "X" | let_cause == "Y" ~ "external", 
    let_cause == "U" & num2_cause %in% 7 ~ "covid19",
    TRUE ~ "other"),
    sex = case_when(sex == 1 ~ "m",
                    sex == 2 ~ "f",
                    TRUE ~ "o")) %>% 
  group_by(country, year, cause, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()

dt5 <- 
  dt4 %>% 
  bind_rows(dt4 %>% 
              group_by(country, year, cause, age) %>% 
              summarise(dts = sum(dts)) %>% 
              ungroup() %>% 
              mutate(sex = "t"))

dt6 <- 
  dt5 %>% 
  bind_rows(dt5 %>% 
              group_by(country, year, sex, age) %>% 
              summarise(dts = sum(dts)) %>% 
              ungroup() %>% 
              mutate(cause = "total")) %>% 
  arrange(country, cause, sex, age, year)

write_rds(dt6, "data_inter/dts_causes_who.rds")


