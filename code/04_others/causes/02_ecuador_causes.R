library(here)
source(here("Code", "00_functions.R"))

# Ecuador mortality data
# ~~~~~~~~~~~~~~~~~~~~~
# # files from Ecuador (Version 1) as of 15 June 2021 
# tabulados_y_series_edg_2020_csv_v1.zip

db_ec_cause <- 
  read_xlsx("Data/Ecuador/ecuador_deaths_cause_age.xlsx")

db_ec_cause2 <- 
  db_ec_cause %>% 
  gather(-Year, -Age, key = Cause, value = Deaths) %>% 
  mutate(Deaths = ifelse(Deaths == "-", "0", Deaths),
         Deaths = Deaths %>% as.integer()) %>% 
  filter(Age != "to",
         Cause != "Total Nacional (t+1)2/") %>% 
  mutate(Cause = paste0("c", str_sub(Cause, 1, 2) %>% str_trim()),
         Age = Age %>% as.integer())

db_ec_cause3 <- 
  db_ec_cause2 %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age >= 1 & Age <= 4 ~ 1,
                         Age >= 5 & Age <= 9 ~ 5,
                         Age >= 10 & Age <= 14 ~ 10,
                         Age >= 15 & Age <= 24 ~ 15,
                         TRUE ~ 99)) %>% 
  group_by(Age, Year, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age < 99)

# age <- 10

# children 1-4
db_child <- 
  db_ec_cause3 %>% 
  # filter(Age == age) %>% 
  group_by(Year, Age) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Cause, Age) %>% 
  mutate(trend = Deaths / mean(Deaths),
         ch = Deaths / lag(Deaths),
         ch_prop_year = prop_year / lag(prop_year)) %>% 
  ungroup() %>% 
  mutate(Cod = str_sub(Cause, 1, 2) %>% str_trim())

db_child2 <- 
  db_child %>% 
  group_by(Year, Age, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  complete(Year, Age, Cause, fill = list(Deaths = 0)) %>% 
  group_by(Year, Age) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Cause, Age) %>% 
  mutate(trend = Deaths / mean(Deaths),
         ch = Deaths / lag(Deaths),
         ch_prop_year = prop_year / lag(prop_year)) %>% 
  ungroup() 

# extrapolating deaths
# ~~~~~~~~~~~~~~~~~~~~

db_child3 <- 
  db_child2 %>% 
  select(Year, Age, Cause, Deaths, prop_year) %>% 
  group_by(Age, Cause) %>% 
  mutate(av_p = mean(prop_year)) %>% 
  ungroup() %>% 
  # mutate(Cause = ifelse(av_p < 0.05, "c88", Cause)) %>% 
  group_by(Year, Age, Cause) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Year >= 2015)

as <- unique(db_child3$Age)

out <- list()

a <- 0
c <- "c46"

for(a in as){
  
  temp <- 
    db_child3 %>% 
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

db_child4 <- 
  out %>% 
  bind_rows() %>% 
  group_by(Year, Age, type) %>% 
  mutate(prop_year = Deaths / sum(Deaths)) %>% 
  ungroup()

# db_child4 %>% 
#   filter(Cause == "c6",
#          Age == 1) %>% 
#   ggplot()+
#   geom_line(aes(Year, Deaths, col = type))

props_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, prop_year, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, prop_year) %>% 
  mutate(ratio_prop = observed / linear)

deaths_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(ratio_prop = observed / linear)

sum_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  group_by(Age, type) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

diffs_2020 <- 
  db_child4 %>% 
  select(Year, Age, Cause, Deaths, type) %>% 
  filter(Year == 2020) %>% 
  spread(type, Deaths) %>% 
  mutate(diff = observed - linear)

conts_2020_ecu <- 
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
         rest = ifelse(lag(cum_cont) < 0.6 | id == 1, 0, 1)) %>% 
  ungroup() %>% 
  mutate(Cause = ifelse(rest == 1, "c88", Cause)) %>% 
  group_by(Age, Cause) %>% 
  summarise(diff = sum(diff)) %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  mutate(cont = diff / sum(diff))


conts_2020_filt_ecu <- 
  conts_2020_ecu %>% 
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


# conts_2020 %>% 
#   ggplot()+
#   geom_bar(aes(x = factor(Age), y = cont, fill = fct_reorder(Cause, -cont)), 
#            stat = "identity", 
#            position="fill")+
#   coord_cartesian(expand = 0)


conts_2020_filt %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = fct_reorder(Cause, -cont)), 
           stat = "identity", 
           position="fill")+
  coord_cartesian(expand = 0)




ggsave(paste0("Figures/causes/cont_cause_ecuador_young.png"), dpi = 600)

write_rds(conts_2020_filt_ecu, "Output/causes_changes_2020_ecuador.rds")




