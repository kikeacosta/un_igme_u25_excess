rm (list = ls()); gc()
source("code/00_functions.R")
DT <- read_rds("data_inter/unpd_deaths_raw.rds")
# ==============================================================================

dts <- 
  DT %>% 
  as_tibble() %>% 
  select(LocID, 
         Country = LocName, 
         Date = TimeStart, 
         Date2 = TimeEnd,
         unit = TimeUnit,
         dura = TimeDuration,
         Sex = SexName, 
         Age = AgeStart, 
         AgeSpan, 
         AgeLabel,
         Deaths = DataValue,
         Source = DataSourceName,
         src_yr = DataSourceYear) %>% 
  mutate(Date = dmy(Date),
         Date2 = dmy(Date2),
         Year = year(Date),
         Year2 = year(Date2),
         Code = countrycode::countrycode(LocID, "un", "iso3c"),
         Sex = case_when(Sex == "Male" ~ "m",
                         Sex == "Female" ~ "f",
                         Sex == "Both sexes" ~ "t",
                         Sex == "Unknown" ~ "unk"),
         Country = recode(Country,
                          "Czech Republic" = "Czechia",
                          "Iran (Islamic Republic of)" = "Iran",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "United States of America" = "USA",
                          "Republic of Korea" = "South Korea",
                          "Russian Federation" = "Russia",
                          "Republic of Moldova" = "Moldova")) %>% 
  drop_na(Sex) %>% 
  select(-Date, -LocID) %>% 
  # only annual data
  filter(unit == "year" & dura == 1) %>% 
  select(-Year2, -unit, -dura, -Date2) %>% 
  # only sources with data in 2020 or 2021
  group_by(Country, Source) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  # adjusting wrong labels
  mutate(AgeLabel = case_when(
    Age == 100 & AgeSpan == 5 & AgeLabel == "99-104" ~ "100-104",
    Age == 105 & AgeSpan == 5 & AgeLabel == "104-109"  ~ "105-109",
    TRUE ~ AgeLabel)) %>% 
  # removing duplicate values
  unique() %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeLabel, AgeSpan) %>% 
  # most recent update of the same source
  filter(src_yr == max(src_yr)) %>% 
  # highest value of deaths
  filter(Deaths == max(Deaths)) %>% 
  ungroup() %>% 
  # excluding HMD, as we are collecting them directly
  filter(Source != "Human Mortality Database") %>% 
  select(-src_yr)

# testing duplicates
dts %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeLabel, AgeSpan) %>% 
  filter(n()>1) %>% 
  ungroup()

# test for data with wrong labels 
dts %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

unique(dts$AgeLabel) %>% sort
unique(dts$Country) %>% sort

# sources and data series
# ~~~~~~~~~~~~~~~~~~~~~~~
srs <- 
  DT %>% 
  select(LocName, DataSourceAuthor, DataSourceName, DataSourceShortName) %>% 
  unique

# ~~~~~~~~~~~~~~~~~~~~~
# Data adjustments ====
# ~~~~~~~~~~~~~~~~~~~~~

# 1. adjust for open age interval (keeping the oldest)
# 2. adjust infant and child ages
# 3. removing incomplete ages
# 4. adjust for totals by age and re-scale
# 5. adjust for totals by sex and re-scale
# 6. harmonize ages (same intervals for all series, max closing at 100+)
# 7. remove series with insufficient periods for baseline (min 3 years 2015-2019)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. adjust for open age interval ==============================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# choosing only one open age interval (highest)
open_ages <- 
  dts %>% 
  filter(AgeSpan == -1 & AgeLabel != "Total") %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  filter(Age == max(Age)) %>% 
  ungroup()

# no open age interval
no_open <- 
  dts %>% 
  anti_join(open_ages %>% 
              select(Country, Source, Sex, Year) %>% 
              unique()) %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()

# unique open age
dts2 <- 
  dts %>% 
  # removing unknown age, total age, and open ages
  filter(AgeSpan >= 0) %>% 
  # adjusting open age interval
  left_join(open_ages %>% 
              select(Country, Source, Year, Sex, max_age = Age)) %>% 
  filter(Age < max_age | is.na(max_age)) %>% 
  # adding open age intervals 
  bind_rows(open_ages) %>% 
  arrange(Country, Source, Year, Sex, Age) %>% 
  select(-max_age) %>% 
  anti_join(no_open)

# test for duplicates 
dts2 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. adjust infant and child ages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# all child ages in infant and 1-4, or 0-4
child_ages <-
  dts2 %>% 
  filter(Age %in% 0:4,
         AgeSpan %in% 1:5) %>% 
  mutate(Age = ifelse(Age %in% 1:4 & AgeSpan == 1, 1, Age),
         AgeLabel = ifelse(Age %in% 1:4 & AgeSpan == 1, "1-4", AgeLabel),
         AgeSpan = ifelse(Age %in% 1:4 & AgeSpan == 1, 4, AgeSpan)) %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeLabel, AgeSpan) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  mutate(has_inf = ifelse(any(AgeLabel == "< 1"), 1, 0),
         has_1_4 = ifelse(any(AgeLabel == "1-4"), 1, 0),
         has_0_4 = ifelse(any(AgeLabel == "0-4"), 1, 0)) %>% 
  ungroup()

child_ages %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)


# combinations to exclude
# ~~~~~~~~~~~~~~~~~~~~~~~
# estimating age 1-4 for those with infant and 0-4 but no 1-4
needed_1_4 <- 
  child_ages %>% 
  filter(has_inf == 1 & has_1_4 == 0 & has_0_4 == 1) %>% 
  select(-starts_with("has_"), -AgeSpan) %>% 
  spread(AgeLabel, Deaths) %>% 
  mutate(`1-4` = `0-4` - `< 1`) %>% 
  gather(`1-4`, `0-4`, `< 1`, key = AgeLabel, value = Deaths) %>% 
  mutate(AgeSpan = case_when(AgeLabel == "< 1" ~ 1,
                             AgeLabel == "1-4" ~ 4,
                             AgeLabel == "0-4" ~ 5)) %>% 
  filter(AgeLabel == "1-4") %>% 
  mutate(Age = 1)

# to exclude with ages 1-4 but no infant nor 0-4
missing_inf <- 
  child_ages %>% 
  filter(has_inf == 0 & has_1_4 == 1 & has_0_4 == 0) %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()

# to exclude with infant but no 1-4 nor 0-4
only_inf <- 
  child_ages %>% 
  filter(has_inf == 1 & has_1_4 == 0 & has_0_4 == 0) %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()

# putting all together
dts_inf_chld <- 
  child_ages %>% 
  select(-starts_with("has")) %>% 
  bind_rows(needed_1_4) %>% 
  anti_join(missing_inf) %>% 
  anti_join(only_inf) %>% 
  arrange(Country, Source, Year, Sex, Age)

# unnecessary ages 0-4
unnecess_0_4 <- 
  dts_inf_chld %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  mutate(has_inf = ifelse(any(AgeLabel == "< 1"), 1, 0),
         has_1_4 = ifelse(any(AgeLabel == "1-4"), 1, 0),
         has_0_4 = ifelse(any(AgeLabel == "0-4"), 1, 0)) %>% 
  ungroup() %>% 
  filter(has_inf == 1 & has_1_4 == 1 & has_0_4 == 1) %>% 
  filter(AgeLabel == "0-4") %>% 
  select(Country, Source, Year, Sex, Age, AgeLabel) %>% 
  unique()

dts_inf_chld2 <- 
  dts_inf_chld %>% 
  anti_join(unnecess_0_4) 

# test of completeness
dts_inf_chld2 %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  mutate(span_inf = ifelse(any(AgeLabel == "< 1"), 1, 0),
         span_1_4 = ifelse(any(AgeLabel == "1-4"), 4, 0),
         span_0_4 = ifelse(any(AgeLabel == "0-4"), 5, 0),
         test = span_inf + span_1_4 + span_0_4) %>% 
  ungroup() %>% 
  filter(test != 5)

# no age zero
no_child <- 
  dts2 %>% 
  anti_join(dts_inf_chld2 %>% 
              select(Country, Source, Sex, Year) %>% 
              unique()) %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()

dts3 <- 
  dts2 %>% 
  filter(Age >= 5) %>% 
  bind_rows(dts_inf_chld2) %>% 
  arrange(Country, Source, Year, Sex, Age) %>% 
  anti_join(no_child)

# test for duplicates 
dts3 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. removing incomplete ages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
incomp_ages <- 
  dts3 %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  mutate(max_age = max(Age),
         sum_span = sum(AgeSpan) + 1,
         test = max_age - sum_span) %>% 
  filter(test != 0) %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()
  
incomp_ages <- 
  dts3 %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  mutate(AgeSpan = ifelse(Age == max(Age), 0, AgeSpan)) %>% 
  filter(max(Age) != sum(AgeSpan)) %>% 
  ungroup() %>% 
  select(Country, Source, Year, Sex) %>% 
  unique()

dts4 <- 
  dts3 %>% 
  anti_join(incomp_ages)

# test for duplicates 
dts4 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. adjust for totals by age and re-scale
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# included series
inc4 <- 
  dts4 %>% 
  select(Country, Code, Source, Year, Sex) %>% 
  unique()


# total ages
# ~~~~~~~~~~
# total ages already in data
age_tot_in <- 
  dts %>% 
  inner_join(inc4) %>%
  filter(AgeLabel == "Total") %>% 
  mutate(Age = "TOT") %>% 
  select(-AgeLabel)

# unknown ages (for combinations without totals)
# ~~~~~~~~~~~~
age_unk_in <- 
  dts %>% 
  inner_join(inc4) %>%
  filter(AgeLabel == "Unknown") %>% 
  filter(Deaths > 0) %>% 
  anti_join(age_tot_in %>% select(Country, Sex, Source, Year))

# no included in data
age_tot_no <- 
  dts4 %>% 
  anti_join(age_tot_in %>% select(Country, Source, Sex, Year) %>% unique()) %>% 
  bind_rows(age_unk_in) %>% 
  group_by(Country, Code, Source, Sex, Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  mutate(Age = "TOT",
         AgeSpan = -1) %>% 
  ungroup()

# putting total ages together
tots_all <- 
  bind_rows(age_tot_in, age_tot_no) %>% 
  arrange(Country, Code, Source, Sex, Year)

# putting all together
dts5 <- 
  dts4 %>% 
  mutate(Age = Age %>% as.character()) %>% 
  bind_rows(tots_all) %>% 
  arrange(Country, Code, Source, Year, Sex, suppressWarnings(as.integer(Age))) 

# re-scalling age
dts6 <- 
  dts5 %>% 
  # anti_join(only_t_sex) %>% 
  group_by(Country, Source, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = Age %>% as.double())

unique(dts6$Age)

# test for duplicates 
dts6 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. adjust for totals by sex and re-scale
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# total sex included series
tot_sex_in <- 
  dts6 %>% 
  filter(Sex == "t") 

# total sex for those missing
tot_sex_no <- 
  dts6 %>% 
  anti_join(tot_sex_in %>% 
              select(Country, Code, Source, Year, Age) %>% 
              unique()) %>% 
  # filter(Sex != "t") %>% 
  group_by(Country, Code, Source, Year, Age, AgeSpan, AgeLabel) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")

# adding total sex
dts7 <- 
  dts6 %>% 
  filter(Sex != "unk" & Sex != "t") %>% 
  bind_rows(tot_sex_in, tot_sex_no) %>% 
  arrange(Country, Code, Source, Year, Sex, suppressWarnings(as.integer(Age))) 

# only with total sex (to exclude)
only_t_sex <- 
  dts7 %>% 
  select(Country, Code, Source, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Code, Source, Year) %>% 
  filter(n() != 3) %>% 
  select(-Sex)

# imputation of unknown sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~
dts8 <-
  dts7 %>% 
  anti_join(only_t_sex) %>% 
  group_by(Country, Source, Age, Year) %>%
  do(rescale_sex(chunk = .data)) %>%
  ungroup()

# adding those with only total sex
dts9 <- 
  dts8 %>% 
  bind_rows(dts7 %>% 
              inner_join(only_t_sex)) %>% 
  arrange(Country, Code, Source, Year, Sex, Age) 

unique(dts9$Sex)

# test for duplicates 
dts9 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. harmonize ages (same open age for all series, max closing at 100+)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

max_age <- 
  dts9 %>% 
  group_by(Country, Code, Source, Sex, Year) %>% 
  summarise(max_age = max(Age)) %>% 
  group_by(Country, Code, Source, Sex) %>% 
  summarise(max_age = min(max_age)) %>% 
  mutate(max_age = min(max_age, 100)) %>% 
  ungroup() 

dts10 <- 
  dts9 %>% 
  left_join(max_age) %>% 
  mutate(Age = ifelse(Age < max_age, Age, max_age),
         AgeLabel = ifelse(Age == max_age, paste0(max_age, "+"), AgeLabel),
         AgeSpan = ifelse(Age == max_age, -1, AgeSpan)) %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeLabel, AgeSpan) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

# test for duplicates 
dts10 %>% 
  group_by(Country, Code, Source, Year, Sex, Age, AgeSpan) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. looking at age groups differences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diff_age_grs <- 
  dts10 %>% 
  # filter(Year < 2020) %>% 
  group_by(Country, Code, Source, Sex, Age) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(Country, Code, Source, Sex, n) %>% 
  unique() %>% 
  group_by(Country, Code, Source, Sex) %>% 
  summarise(grs = n()) %>% ungroup() %>% 
  filter(grs>1) %>% 
  select(-grs)

dts11 <- 
  dts10 %>% 
  anti_join(diff_age_grs)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7. remove series with insufficient periods for baseline (min 3 years 2015-2019)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# series with sufficient years for estimating the baseline (at least 3 within 2015-2019)
# series with data during the pandemic 2020-2021 (at least 1)

dts12 <- 
  dts11 %>% 
  mutate(pre = ifelse(Year %in% 2015:2019, 1, 0),
         pan = ifelse(Year %in% 2020:2021, 1, 0)) %>% 
  group_by(Country, Code, Source, Sex, Age, AgeLabel, AgeSpan) %>% 
  filter(sum(pre) >= 3 & sum(pan) >= 1) %>% 
  ungroup()


sum_dts_source <- function(db){
  db %>%
    filter(Year >= 2015) %>%
    group_by(Country, Source, Sex) %>%
    filter(max(Year) >= 2020) %>%
    ungroup() %>%
    group_by(Country, Source, Year, Age) %>%
    mutate(sexs = n()) %>%
    ungroup() %>%
    group_by(Country, Source, Sex, Age) %>%
    mutate(years = n()) %>%
    ungroup() %>%
    group_by(Country, Source, Sex, Year) %>%
    mutate(ages = n()) %>%
    ungroup() %>%
    group_by(Country, Source) %>%
    filter(!(sexs == 3 & Sex == "t")) %>%
    summarise(Deaths = sum(Deaths),
              sexs = min(sexs),
              years = min(years),
              ages = min(ages),
              Source = unique(Source) %>% paste(collapse = ", ")) %>%
    ungroup() %>%
    unique()
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8. select young ages for UNICEF analysis 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dts_yng <- 
  dts12 %>% 
  filter(Age + AgeSpan <= 25) %>% 
  mutate(age_up = Age + AgeSpan - 1) %>% 
  arrange(Country, Source, Year, Sex, Age) %>% 
  select(-AgeSpan) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9. selecting best source for each country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summ_dts1021 <- sum_dts_source(dts_yng)

comp_dts <- 
  summ_dts1021 %>% 
  unique() %>% 
  mutate(Deaths = round(Deaths)) %>% 
  arrange(Country) %>% 
  # unique sources
  group_by(Country) %>% 
  # max age definition
  filter(years == max(years)) %>% 
  filter(ages == max(ages)) %>%
  filter(sexs == max(sexs)) %>% 
  filter(Deaths == max(Deaths)) %>% 
  # filter((any(Source == "unpd_crvs") & Source == "unpd_crvs") | all(Source != "unpd_crvs")) %>%
  filter((any(Source == "Demographic Yearbook") & Source == "Demographic Yearbook") | all(Source != "Demographic Yearbook")) %>% 
  filter((any(Source == "WHO Mortality Data base") & Source == "WHO Mortality Data base") | all(Source != "WHO Mortality Data base"))
# filter((any(Source_int == "unpd_hmd") & Source_int == "unpd_hmd") | all(Source_int != "unpd_hmd")) %>%
  # # filter(!(n() == 2 & Source %in% c("who_mort_db", "unpd", "hmd"))) %>% 
  # mutate(best = ifelse(n() == 1, Source, NA))

sel_dts <- 
  comp_dts %>% 
  select(Country, Source)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10. standardize sources 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_yng2 <- 
  dts_yng %>% 
  inner_join(sel_dts) %>% 
  mutate(Deaths = round(Deaths),
         Source_int = case_when(
           Source %in% c("WHO-IGME Mortality Data base") ~ "unpd_who_mdb",
           Source %in% c("WHO Country Consultation",
                         "WHO All-Cause Mortality Data Call") ~ "unpd_who_ccd",
           Source %in% c("Demographic Yearbook") ~ "unpd_dy",
           Source %in% c("Human Mortality Database") ~ "unpd_hmd",
           Source %in% c("Latin American Mortality Database 2 (LAMBdA)") ~ "unpd_lambda",
           TRUE ~ "unpd_crvs")) %>% 
  unique() %>% 
  select(-AgeLabel) %>% 
  filter(Source != "unpd_hmd")

# dts_yng3 <- read_rds("data_inter/unpd.rds")
out_dts <- 
  dts_yng2 %>% 
  ungroup() %>% 
  select(-Source) %>% 
  rename(Source = Source_int)
  
unique(out_dts$Country)
  
# saving output
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data sources
write_rds(dts_yng2, "data_inter/sources_unpd_dts.rds")
# deaths data
write_rds(out_dts, "data_inter/unpd.rds")

