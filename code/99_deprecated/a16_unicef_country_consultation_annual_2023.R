# rm(list = ls())
# source("Code/00_functions.R")
# 
# ccd_name1 <- "Data/unicef/country_consultation/dth0yrDBwide_20231207.csv"
# ccd_name2 <- "Data/unicef/country_consultation/MasterMDB_20231207.csv"
# ccd_name3 <- "Data/unicef/country_consultation/SBR_data_cc 2023.xlsx"
# 
# d1 <- read_csv(ccd_name1)
# d2 <- read_csv(ccd_name2)
# d3 <- read_xlsx(ccd_name3)
# 
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # infant and neonatal deaths ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# unique(d1$d999)
# # d28 refers to postneonatal deaths
# # Y0 refers to all infant deaths
# # d0+d1+d7 refers to neonatal deaths
# 
# neo1 <- 
#   d1 %>% 
#   mutate(d999 = ifelse(is.na(d999), 0, d999),
#          t1 = d0+d1+d7+d28+d999,
#          diff = y0-t1)
# neo1 %>% 
#   filter(diff != 0)
# 
# # issues so far 
# # ~~~~~~~~~~~~~
# # wrong total infants for Peru (re-estimating them here)
# # only totals for Peru and the Netherlands in 2021 (excluded)
# 
# neo2 <- 
#   neo1 %>% 
#   filter(year >= 2010) %>% 
#   group_by(iso3) %>% 
#   filter(max(year) >= 2020) %>% 
#   replace_na(list(d0 = 0, d1 = 0, d7 = 0, d28 = 0)) %>% 
#   mutate(y0 = ifelse(iso3 == "PER", d0+d1+d7+d28+d999, y0),
#          neo = d0+d1+d7,
#          neo2 = ifelse(y0>0, neo*y0/(d0+d1+d7+d28), 0),
#          diff_neo = neo-neo2)
# 
# neo3 <- 
#   neo2 %>%
#   select(code = iso3, country = whoname, year,
#          inf = y0, neo = neo2, Births = lb) %>% 
#   filter(!(country == "Italy" & year == 2021),
#          !(country == "Netherlands" & year == 2021))
# 
# neo <- 
#   neo3 %>% 
#   select(code, country, year, value = neo) %>% 
#   mutate(measure = "neo",
#          Source = "unicef_ccd23")
# 
# 
# 
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# # ~~~~~~~~~~~
# # births ====
# # ~~~~~~~~~~~
# 
# bts <- 
#   neo3 %>% 
#   select(Code = code, Country = country, Year = year, Births) %>% 
#   mutate(Source = "unicef_ccd23")
# 
# write_rds(bts, "data_inter/annual_births_ccd23.rds")
# 
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # ~~~~~~~~~~~~~~~~
# # stillbirths ====
# # ~~~~~~~~~~~~~~~~
# sbs <- 
#   d3 %>% 
#   select(code = iso3, 
#          year = 5, 
#          neo,
#          bts = lb,
#          sbs = sb) %>% 
#   gather(neo, sbs, key = measure, value = value) %>% 
#   drop_na() %>% 
#   mutate(source = "unicef_ann23",
#          country = countrycode(code, origin = "iso3c",
#                                destination = "country.name"))
# 
# write_rds(sbs, "data_inter/neo_sbs_annual_unicef23.rds")
# 
# 
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # ~~~~~~~~~~~~~~~~~
# # youth deaths ====
# # ~~~~~~~~~~~~~~~~~
# 
# unique(d2$comments)
# unique(d2$var)
# 
# pop <- 
#   d2 %>% 
#   filter(var == "pop") %>% 
#   gather(starts_with("x"), key = age, value = pop) %>% 
#   select(iso3, year, sex, age, pop)
# 
# unique(d2$sex)
# 
# dt <- 
#   d2 %>% 
#   filter(var == "dth",
#          sex != "b",
#          year >= 2010) %>% 
#   group_by(iso3, sex) %>% 
#   filter(max(year) >= 2020) %>% 
#   gather(starts_with("x"), key = age, value = dts) %>% 
#   replace_na(list(dts = 0)) %>% 
#   select(iso3, year, sex, age, dts, total) %>% 
#   group_by(iso3, year, sex) %>% 
#   mutate(dts_tot = sum(dts)) %>% 
#   mutate(diff = dts_tot - total)
# 
# # several differences adding the totals together,
# # taking Total value by sex (for now) and redistributing deaths
# dt2 <- 
#   dt %>% 
#   filter(age != "x999") %>% 
#   group_by(iso3, year, sex) %>% 
#   mutate(dts_tot = sum(dts),
#          dx = ifelse(total == 0 | dts_tot == 0, 
#                      0,
#                      dts*total/dts_tot)) %>% 
#   select(iso3, year, sex, age, dx) %>% 
#   mutate(age = str_remove(age, "x"),
#          age = age %>% as.double())
# 
# # re-scaling sex
# dt3 <- 
#   dt2 %>% 
#   group_by(iso3, year, age) %>% 
#   mutate(dts = sum(dx)) %>% 
#   filter(sex != "u") %>% 
#   mutate(dx = ifelse(dts > 0 & dx > 0, 
#                      dx*dts/sum(dx),
#                      0)) %>% 
#   ungroup() %>% 
#   select(-dts)
# 
# # taking only under-25 and grouping ages
# dt4 <- 
#   dt3 %>% 
#   filter(age <= 24) %>% 
#   mutate(age = case_when(age == 0 ~ 0,
#                          age %in% 1:4 ~ 1,
#                          age %in% 5:24 ~ age-age%%5)) %>% 
#   group_by(iso3, year, sex, age) %>% 
#   summarise(dx = sum(dx)) %>% 
#   ungroup()
# 
# # adding total sex and out
# dt5 <- 
#   dt4 %>% 
#   bind_rows(dt4 %>% 
#               group_by(iso3, year, age) %>% 
#               summarise(dx = sum(dx),
#                         sex = "t") %>% 
#               ungroup()) %>% 
#   drop_na() %>% 
#   mutate(Country = countrycode(iso3, origin = "iso3c",
#                                destination = "country.name"),
#          Country = case_when(iso3 == "RKS" ~ "Kosovo",
#                              TRUE ~ Country)) %>% 
#   select(Code = iso3,
#          Country,
#          Year = year,
#          Sex = sex,
#          Age = age,
#          Deaths = dx) %>% 
#   mutate(Source = "ccd23") %>% 
#   arrange(Code, Sex, Age, Year)
# 
# write_rds(dt5, "data_inter/ccd_2023.rds")
# 
# # TODO: pending to look at sources here!!!!
# 
# # Adding information on sources
# scr <- 
#   d2 %>% 
#   filter(var == "dth",
#          year >= 2010,
#          sex %in% c("m", "f")) %>% 
#   select(iso3, year, source = from) %>% 
#   unique() %>% 
#   mutate(source = ifelse(str_detect(source, "cc"), "ccd", str_to_lower(source)))
# 
# scr %>% 
#   group_by(iso3, year) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1)
# 
# scr2 <- 
#   scr %>% 
#   group_by(iso3, year) %>% 
#   summarise(source = unique(source) %>% paste(collapse = ", ")) %>% 
#   ungroup()
#   
