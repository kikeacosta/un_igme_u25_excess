rm (list = ls())
source("Code/00_functions.R")
library(readxl)

db_in2 <- 
  read_xlsx("Data/HMIS/20220830_Covid analysis data_HMIS.xlsx",
            sheet = 1) 

unique(db_in2$whoname)

bts <- 
  db_in2 %>% 
  select(iso3, whoname, year, quarter, month, lb) %>% 
  drop_na(lb)

neo <- 
  db_in2 %>% 
  select(iso3, whoname, year, quarter, month, ndth) %>% 
  drop_na(ndth)

sbs <- 
  db_in2 %>% 
  select(iso3, whoname, year, quarter, month, sb_x28wks) %>% 
  drop_na(sb_x28wks)

inf <- 
  db_in2 %>% 
  select(iso3, whoname, year, quarter, month, d0) %>% 
  drop_na(d0)

chd <- 
  db_in2 %>% 
  select(iso3, whoname, year, quarter, month, d0_4) %>% 
  drop_na(d0_4)
