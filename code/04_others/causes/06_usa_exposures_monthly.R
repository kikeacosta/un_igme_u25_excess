source("Code/causes/00_functions_cause_analyses.R")

pop <- 
  read_tsv("Data/USA/wonder_under_25_annual.txt") %>% 
  select(age = 3,
         year = 4,
         pop = 7) %>% 
  drop_na(year) 

ags <- unique(pop$age)

pop2 <- 
  pop %>% 
  mutate(date = make_date(d = 15, m = 6, y = year),
         pop = pop / 12) %>% 
  select(-year) %>%
  complete(age = ags,
           date = seq(ymd('2014-06-15'),ymd('2020-12-15'), by = 'month'),
           fill = list(pop = NA)) %>% 
  arrange(age, date) %>% 
  group_by(age) %>% 
  mutate(t = 1:n())

inter_exposures <- function(db){
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t)
  db %>% 
    mutate(exposure = spline(xs, ys, xout = ts)$y)
}

pop3 <- 
  pop2 %>% 
  group_by(age) %>% 
  do(inter_exposures(db = .)) %>% 
  ungroup() %>% 
  select(age, date, exposure)


# from STFF database
# ~~~~~~~~~~~~~~~~~~
# loading data from Human Fertility Database (HFD)
db_stff <- read_csv("https://www.humanfertility.org/STFF/stff.csv")

bts <- 
  db_stff %>% 
  filter(CountryCode == "USA") %>% 
  select(1:15, -Area) %>% 
  filter(Year %in% 2015:2020) %>% 
  gather(-CountryCode, -Year, key = month, value = exposure) %>% 
  mutate(mth = as.integer(factor(month, levels = month.name)),
         date = paste(15, mth, Year, sep = "-") %>% dmy()) %>% 
  select(date, exposure) %>% 
  mutate(age = "0-1",
         exposure = exposure %>% as.double())

pop4 <- 
  pop3 %>% 
  filter(age != "1") %>% 
  filter(date >= "2015-01-01") %>% 
  bind_rows(bts) %>% 
  mutate(age = factor(age, 
                      levels = c("0-1", "1-4", "5-9", "10-14", "15-19", "20-24"))) %>% 
  arrange(age, date)

write_rds(pop4, "output/usa/exposures_young_ages_monthly.rds")
