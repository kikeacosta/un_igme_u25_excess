library(read.dbc)
library(tidyverse)
library(here)
library(lubridate)

reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

links <- paste0(here("Data", "Brazil", "deaths"), "/Mortalidade_Geral_", 2015:2021, ".csv")

i <- links[1]
out <- list()
for (i in links){
  cat(i)
  out[[i]] <- read_delim(i, 
                         delim = ";",
                         col_types = cols(.default = "c")) %>%  
    filter(TIPOBITO == "2", 
           OBITOGRAV == "1", 
           IDADE %in% as.character(410:450)) %>% 
    select(date_e = DTOBITO, mun_code = CODMUNOCOR, type = TPMORTEOCO) %>% 
    mutate(date_e = dmy(date_e),
           mth = month(date_e),
           year = year(date_e),
           date = make_date(d = 15, m = mth, y = year),
           state_num = str_sub(mun_code, 1, 2) %>% as.double()) %>% 
    group_by(date, state_num, type) %>% 
    summarise(dts = n()) %>% 
    ungroup()
}

dts <- 
  out %>% 
  bind_rows() %>% 
  left_join(reg_codes)

dts_reg <- 
  dts %>% 
  group_by(date, region) %>% 
  summarise(dts = sum(dts))

dts_nal <- 
  dts %>% 
  group_by(date) %>% 
  summarise(dts = sum(dts))

dts %>%
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~state_iso, scales = "free")

dts_reg %>%
  ggplot()+
  geom_point(aes(date, dts))+
  facet_wrap(~region, scales = "free")


dts_nal %>% 
  ggplot()+
  geom_point(aes(date, dts))
