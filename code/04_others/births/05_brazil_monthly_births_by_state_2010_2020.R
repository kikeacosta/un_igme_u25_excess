library(here)
source(here("Code", "00_functions.R"))

# births 
# ===========
bts <- 
  read_csv2("Data/Brazil/births/monthly_births_state_2010_2019.csv",
            skip = 3)

bts2 <- 
  bts %>% 
  drop_na(RO) %>% 
  rename(period = 1) %>% 
  separate(period, c("month", "year"), sep = "/") %>% 
  filter(month != "Total",
         !month %in% as.character(2010:2019)) %>% 
  fill(year) %>% 
  group_by(year) %>% 
  mutate(mth = 1:n(),
         date = make_date(d = 15, m = mth, y = year)) %>% 
  ungroup() %>% 
  select(-year, -month, -mth, -Total) %>% 
  gather(-date, key = state_iso, value = bts) %>% 
  left_join(reg_codes)


bts_20 <- 
  read_csv2("Data/Brazil/births/monthly_births_state_2020.csv",
            skip = 3) %>% 
  drop_na(RO) %>% 
  rename(period = 1) %>% 
  separate(period, c("month", "year"), sep = "/") %>% 
  filter(month != "Total",
         !month %in% as.character(2020)) %>% 
  fill(year) %>% 
  group_by(year) %>% 
  mutate(mth = 1:n(),
         date = make_date(d = 15, m = mth, y = year)) %>% 
  ungroup() %>% 
  select(-year, -month, -mth, -Total) %>% 
  gather(-date, key = state_iso, value = bts) %>% 
  left_join(reg_codes)

bts_10_20 <- 
  bind_rows(bts2, bts_20) %>% 
  arrange(state_iso, date) %>% 
  select(state_iso, state_num, state_name, region, date, bts)

# saving estimates
# ~~~~~~~~~~~~~~~~
write_rds(bts_10_20, "Output/births/brazil_monthly_births_state.rds")
