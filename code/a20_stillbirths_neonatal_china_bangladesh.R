rm (list = ls())
source("code/00_functions.R")

cn_bd <- read_xlsx("data_input/unicef/chn_bgd/SVR_BGD CHN.xlsx")

cn_bd2 <- 
  cn_bd %>% 
  select(code = 1, country = 2,year = 10,
         value = 13) %>% 
  mutate(measure = "neo_r",
         source = "svr")

write_rds(cn_bd2, "data_inter/neo_sbs_chn_bgd.rds")
