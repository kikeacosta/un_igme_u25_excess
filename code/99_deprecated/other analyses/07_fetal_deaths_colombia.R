library(here)
source(here("Code", "00_functions.R"))
library(haven)

# stillbirths in Colombia

# loading data
# microdata 2015-2019
db_2015 <- read_sav("Data/Colombia/stillbirths/Fetal_2015.sav") %>% 
  as_tibble()
db_2016 <- read_csv("Data/Colombia/stillbirths/Fetal_2016.csv")
db_2017 <- read_csv2("Data/Colombia/stillbirths/fetal2017.csv")
db_2018 <- read_csv2("Data/Colombia/stillbirths/fetal2018.csv")
db_2019 <- read_csv("Data/Colombia/stillbirths/Fetales_2019.csv")

# Aggregated by quarter for 2020 and 2021(I)
db_2020_q1 <- 
  read_xls("Data/Colombia/stillbirths/CUADRO4-FETALES-2020-PRELIMINAR.xls",
           skip = 11) %>% 
  rename(dpto = 1,
         age_mother = 2,
         total = 3) %>% 
  drop_na(age_mother) %>% 
  fill(dpto) %>% 
  gather(-dpto, -age_mother, key = weeks, value = stbs) %>% 
  mutate(qt = 1)
  
db_2020_q2 <- 
  read_xls("Data/Colombia/stillbirths/CUADRO4-FETALES-2020-II-pr.xls",
                       skip = 11) %>% 
  rename(dpto = 1,
         age_mother = 2,
         total = 3) %>% 
  drop_na(age_mother) %>% 
  fill(dpto) %>% 
  gather(-dpto, -age_mother, key = weeks, value = stbs) %>% 
  mutate(qt = 2)

db_2020_q3 <- 
  read_xlsx("Data/Colombia/stillbirths/defunciones-fetales2020p-cuadro-definitivo-III-2020.xlsx",
            sheet = "Cuadro4",
            skip = 8) %>% 
  select(-1) %>% 
  rename(dpto = 1,
         age_mother = 2,
         total = 3) %>% 
  drop_na(age_mother) %>% 
  fill(dpto) %>% 
  gather(-dpto, -age_mother, key = weeks, value = stbs) %>% 
  mutate(qt = 3)

db_2020_q4 <- 
  read_xlsx("Data/Colombia/stillbirths/defunciones-fetales2020p-cuadro-definitivo-IV-2020.xlsx",
            sheet = "Cuadro4",
            skip = 8) %>% 
  select(-1) %>% 
  rename(dpto = 1,
         age_mother = 2,
         total = 3) %>% 
  drop_na(age_mother) %>% 
  fill(dpto) %>% 
  gather(-dpto, - age_mother, key = weeks, value = stbs) %>% 
  mutate(qt = 4)

db_2021_q1 <- 
  read_xlsx("Data/Colombia/stillbirths/defunciones-fetales2021p-cuadro-unificado-I-2021.xlsx",
            sheet = "Cuadro4",
            skip = 8) %>% 
  select(-1) %>% 
  rename(dpto = 1,
         age_mother = 2,
         total = 3) %>% 
  drop_na(age_mother) %>% 
  fill(dpto) %>% 
  gather(-dpto, - age_mother, key = weeks, value = stbs) %>% 
  mutate(qt = 1)


# appending all quarters in 2020 and 2021
db_2020 <- 
  bind_rows(db_2020_q1,
            db_2020_q2,
            db_2020_q3,
            db_2020_q4) %>% 
  mutate(year = 2020) %>% 
  bind_rows(db_2021_q1 %>% 
              mutate(year = 2021)) %>% 
  mutate(dpto = recode(dpto,
                       "TOTAL NACIONAL" = "total",
                       "Total Nacional" = "total",
                       "Sin Información" = "Sin información",
                       "Extranjeros" = "Extranjero"),
         weeks = case_when(weeks %in% c("Menos de 22", "De 22 a 27") ~ "0_27",
                           weeks %in% c("De 28 a 36", "De 37 y más") ~ "28+",
                           weeks %in% c("Ignorado", "Sin información") ~ "unk",
                           weeks %in% c("total") ~ "total")) %>% 
  group_by(year, qt, dpto, age_mother, weeks) %>% 
  summarise(stbs = sum(stbs)) %>% 
  ungroup() %>% 
  filter(age_mother == "Total") %>% 
  select(-age_mother) %>% 
  mutate(dpto = )

unique(db_2020$dpto)
unique(db_2020$age_mother)
unique(db_2020$weeks2)
unique(db_2020$dpto)

unique(db_2015$T_GES)
unique(db_2015$MES)
unique(db_2015$COD_DPTO)
unique(db_2015$EDAD_MADRE)

# aggregating microdata 2015-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# variable labels
cod_dptos <- 
  stack(attr(db_2015$CODPTORE, 'labels')) %>% 
  as_tibble() %>%
  rename(cod_dpto = values,
         dpto = ind) %>% 
  mutate(dpto = dpto %>% as.character() %>% str_trim(),
         cod_dpto = cod_dpto %>% as.double())

cod_weeks <- 
  stack(attr(db_2015$T_GES, 'labels')) %>% 
  as_tibble() %>%
  rename(cod_weeks = values,
         weeks = ind) %>% 
  mutate(weeks = weeks %>% as.character() %>% str_trim(),
         cod_weeks = cod_weeks %>% as.double())

cod_age_mother <- 
  stack(attr(db_2015$EDAD_MADRE, 'labels')) %>% 
  as_tibble() %>%
  rename(cod_age_mother = values,
         age_mother = ind) %>% 
  mutate(age_mother = age_mother %>% as.character() %>% str_trim(),
         cod_age_mother = cod_age_mother %>% as.double())

# function to aggregate microdata
format_stbs <- function(db){
  db2 <- 
    db %>% 
    mutate(mes = as.double(as.character(MES)),
           cod_dpto = as.double(as.character(CODPTORE)),
           qt = case_when(mes %in% 1:3 ~ 1,
                          mes %in% 4:6 ~ 2,
                          mes %in% 7:9 ~ 3,
                          mes %in% 10:12 ~ 4),
           cod_age_mother = as.double(as.character(EDAD_MADRE)),
           cod_weeks = as.double(as.character(T_GES))) %>% 
    group_by(cod_dpto, cod_age_mother, qt, cod_weeks) %>% 
    summarise(stbs = n()) %>% 
    ungroup() %>% 
    left_join(cod_dptos) %>% 
    left_join(cod_weeks) %>% 
    left_join(cod_age_mother) %>% 
    select(dpto, age_mother, weeks, qt, stbs)
  
  db_all_age_mothers <- 
    db2 %>% 
    group_by(dpto, qt, weeks) %>% 
    summarise(stbs = sum(stbs)) %>% 
    ungroup() %>% 
    mutate(age_mother = "total")
  
  db_all_weeks <- 
    db2 %>% 
    bind_rows(db_all_age_mothers) %>% 
    group_by(dpto, qt, age_mother) %>% 
    summarise(stbs = sum(stbs)) %>% 
    ungroup() %>% 
    mutate(weeks = "total")
  
  db_national <- 
    db2 %>% 
    bind_rows(db_all_age_mothers) %>% 
    bind_rows(db_all_weeks) %>% 
    group_by(age_mother, qt, weeks) %>% 
    summarise(stbs = sum(stbs)) %>% 
    ungroup() %>% 
    mutate(dpto = "total")
  
  out <- 
    bind_rows(db2,
              db_national,
              db_all_age_mothers,
              db_all_weeks)
  
  return(out)
    
}

db_2015_2 <- 
  format_stbs(db_2015) %>% 
  mutate(year = 2015)

db_2016_2 <- 
  format_stbs(db_2016) %>% 
  mutate(year = 2016)

db_2017_2 <- 
  format_stbs(db_2017) %>% 
  mutate(year = 2017)

db_2018_2 <- 
  format_stbs(db_2018) %>% 
  mutate(year = 2018)

db_2019_2 <- 
  format_stbs(db_2019) %>% 
  mutate(year = 2019)

db_15_19 <- 
  bind_rows(db_2015_2,
            db_2016_2,
            db_2017_2,
            db_2018_2,
            db_2019_2) %>% 
  filter(age_mother == "total") %>% 
  mutate(weeks = str_replace(weeks, " semanas", ""),
         weeks = case_when(weeks %in% c("Menos de 22", "De 22 a 27") ~ "0_27",
                           weeks %in% c("De 28 a 37", "De 38 a 41", "De 42 y más") ~ "28+",
                           weeks %in% c("Sin información", "Ignorado") ~ "unk",
                           TRUE ~ weeks),
         dpto = case_when(is.na(dpto) | dpto == "Sin información de departamento" ~ "unk",
                          dpto == "Con residencia en el extranjero" ~ "ext",
                          TRUE ~ dpto)) %>% 
  group_by(year, qt, dpto, weeks) %>% 
  summarise(stbs = sum(stbs)) %>% 
  ungroup()

unique(db_15_19$dpto)
unique(db_15_19$weeks)

# function to distribute unknowns
dist_wks_unk <- function(chunk){
  
  total <- 
    chunk %>% 
    filter(weeks == "total") %>% 
    pull(stbs)
  
  fct_adj <- 
    chunk %>% 
    filter(!weeks %in% c("total", "unk")) %>% 
    summarise(fct_adj = total / sum(stbs)) %>% 
    pull(fct_adj)
  
  db2 <- 
    chunk %>% 
    filter(!weeks %in% c("unk")) %>% 
    mutate(stbs = ifelse(weeks == "total", stbs, stbs * fct_adj))
  
  return(db2)

}

db_15_19_2 <-
  db_15_19 %>% 
  group_by(year, qt, dpto) %>% 
  do(dist_wks_unk(chunk = .data)) %>% 
  ungroup()
  
# national database
# ~~~~~~~~~~~~~~~~~
db_15_19_nal <- 
  db_15_19_2 %>% 
  filter(dpto == "total") 


db_2020_2 <- 
  db_2020 %>% 
  group_by(year, qt, dpto) %>% 
  do(dist_wks_unk(chunk = .data)) %>% 
  ungroup()

db_2020_nal <- 
  db_2020_2 %>% 
  filter(dpto == "total") 

db_15_21_nal <- 
  bind_rows(db_15_19_nal,
            db_2020_nal) %>% 
  mutate(month = case_when(qt == 1 ~ 2,
                           qt == 2 ~ 5,
                           qt == 3 ~ 8,
                           qt == 4 ~ 11),
         date = make_date(d = 15, m = month, y = year)) 


db_15_21_nal %>% 
  ggplot()+
  geom_point(aes(date, stbs))+
  geom_line(aes(date, stbs))+
  facet_wrap(~ weeks, scales = "free")+
  theme_bw()

# ===================


# births
# ===================
bs_2020_1 <- 
  read_xls("Data/Colombia/births/Cuadro1-NACIMIENTOS-2020-PRELIMINAR.xls",
           skip = 10) %>% 
  drop_na() %>% 
  select(age_mother = 1,
         births = 2) %>% 
  filter(age_mother == "TOTAL NACIONAL") %>% 
  mutate(year = 2020,
         qt = 1)

bs_2020_2 <- 
  read_xls("Data/Colombia/births/Cuadro1-NACIMIENTOS-2020-II-pr.xls",
           skip = 10) %>% 
  drop_na() %>% 
  select(age_mother = 1,
         births = 2) %>% 
  filter(age_mother == "TOTAL NACIONAL") %>% 
  mutate(year = 2020,
         qt = 2)

bs_2020_3 <- 
  read_xlsx("Data/Colombia/births/nacimientos2020p-cuadro-definitivo-III-2020.xlsx",
            sheet = "Cuadro1",
            skip = 7) %>% 
  drop_na() %>% 
  select(age_mother = 1,
         births = 2) %>% 
  filter(age_mother == "TOTAL NACIONAL") %>% 
  mutate(year = 2020,
         qt = 3)

bs_2020_4 <- 
  read_xlsx("Data/Colombia/births/nacimientos2020p-cuadro-definitivo-IV-2020.xlsx",
            sheet = "Cuadro1",
            skip = 7) %>% 
  drop_na() %>% 
  select(age_mother = 1,
         births = 2) %>% 
  filter(age_mother == "Total Nacional") %>% 
  mutate(year = 2020,
         qt = 4)

bs_2021_1 <- 
  read_xlsx("Data/Colombia/births/nacimientos2021p-cuadro-unificado-2021-Itrim.xlsx",
            sheet = "Cuadro1",
            skip = 7) %>% 
  drop_na() %>% 
  select(age_mother = 1,
         births = 2) %>% 
  filter(age_mother == "Total Nacional") %>% 
  mutate(year = 2021,
         qt = 1)


# microdata births 2015-2019
db_b2015 <- read_sav("Data/Colombia/births/Nac_2015.sav") %>% 
  as_tibble()
# db_b2016 <- read_sav("Data/Colombia/births/Nac_2016.sav") %>% 
#   as_tibble()
db_b2016 <- read_csv("Data/Colombia/births/Nac_2016.csv")
db_b2017 <- read_csv2("Data/Colombia/births/nac2017.csv")
db_b2018 <- read_csv2("Data/Colombia/births/nac2018.csv")
db_b2019 <- read_csv("Data/Colombia/births/Nacidos_2019.csv")


format_births<- function(db){
  db2 <- 
    db %>% 
    mutate(mes = as.double(as.character(MES)),
           cod_dpto = as.double(as.character(CODPTORE)),
           qt = case_when(mes %in% 1:3 ~ 1,
                          mes %in% 4:6 ~ 2,
                          mes %in% 7:9 ~ 3,
                          mes %in% 10:12 ~ 4)) %>% 
    group_by(cod_dpto, qt) %>% 
    summarise(births = n()) %>% 
    ungroup() %>% 
    left_join(cod_dptos) %>% 
    select(dpto, qt, births)
  
  db_national <- 
    db2 %>% 
    group_by(qt) %>% 
    summarise(births = sum(births)) %>% 
    ungroup() %>% 
    mutate(dpto = "total")
  
  out <- 
    bind_rows(db2,
              db_national)
  
  return(out)
  
}

db_b2015_2 <- 
  format_births(db_b2015) %>% 
  mutate(year = 2015)

db_b2016_2 <- 
  format_births(db_b2016) %>% 
  mutate(year = 2016)

db_b2017_2 <- 
  format_births(db_b2017) %>% 
  mutate(year = 2017)

db_b2018_2 <- 
  format_births(db_b2018) %>% 
  mutate(year = 2018)

db_b2019_2 <- 
  format_births(db_b2019) %>% 
  mutate(year = 2019)

db_b15_19 <- 
  bind_rows(db_b2015_2,
            db_b2016_2,
            db_b2017_2,
            db_b2018_2,
            db_b2019_2) %>% 
  mutate(dpto = case_when(is.na(dpto) | dpto == "Sin información de departamento" ~ "unk",
                          dpto == "Con residencia en el extranjero" ~ "ext",
                          TRUE ~ dpto))

unique(db_b15_19$dpto) %>% sort()

db_b15_19_nal <- 
  db_b15_19 %>% 
  filter(dpto == "total") %>% 
  select(-dpto)

bts_15_21 <- 
  bind_rows(bs_2020_1,
            bs_2020_2,
            bs_2020_3,
            bs_2020_4,
            bs_2021_1) %>% 
  select(-age_mother) %>% 
  bind_rows(db_b15_19_nal)

bts_15_21 %>% 
  mutate(date = paste(year, qt, sep = "_")) %>% 
  ggplot()+
  geom_point(aes(date, births)) 
# ===================


# merging fetal deaths and births ====
# ====================================
db <- 
  db_15_21_nal %>% 
  left_join(bts_15_21) %>% 
  mutate(mx = stbs / (births + stbs))

db %>% 
  ggplot()+
  geom_point(aes(date, mx))+
  geom_line(aes(date, mx))+
  facet_wrap(~ weeks, scales = "free")+
  theme_bw()
ggsave("Figures/fetal_deaths/colombia_quarters_15_21.png", dpi = 1000)

write_rds(db, "Output/stillbirths/colombia_trim_stillbirths_births_2015_2021.rds")


# annual to contrast
db_annual <- 
  db %>% 
  group_by(year, weeks) %>% 
  summarise(stbs = sum(stbs),
            births = sum(births))


# contrast with UNICEF data
unicef_sbts <- 
  read_xlsx("Data/other_unicef/UNIGME_2020_SBR_admindata_BRA_COL.xlsx",
            sheet = "Colombia",
            skip = 1) %>% 
  filter(Definition_SB == "x28wks") %>% 
  select(year = Reference_year, lb, tb, sb, sbr_rec, neo, infant) %>% 
  left_join(db_annual %>%
              filter(weeks == "28+") %>% 
              select(year, stbs, births)) %>% 
  mutate(diff = stbs - sb,
         ratio = stbs / sb)

# Note:
# stillbirths processed here are considerably higher (14%-30%) compared to those 
# reported in the UNICEF file. Wheres here, unknown weeks are imputed 
# accordingly to the distribution that is known in data, UNICEF data is not 
# adjusted. This adjustment explains the difference


# ===================

# excess fetal deaths 
# ===================
# quasi-Poisson model to analyze excess fetal deaths 
chunk <- 
  db %>% 
  filter(weeks == "28+") %>% 
  select(date, qt, stbs, births) %>% 
  mutate(exposure = stbs + births,
         t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0))

model <- gam(stbs ~ t + s(qt, bs = 'cp') +
               offset(log(exposure)),
             data = chunk,
             weights = w,
             family = quasipoisson(link = "log"))

resp <- predict(model, newdata = chunk, type = "response", se.fit = TRUE)

db_baseline <- 
  chunk %>%
  mutate(baseline = resp$fit,
         ul = baseline + 1.96 * resp$se,
         ll = baseline - 1.96 * resp$se,
         p_score = stbs / baseline,
         stbs_r = stbs / exposure,
         bs_r = baseline / exposure,
         ll_r = ll / exposure,
         ul_r = ul / exposure)

db_baseline %>% 
  ggplot()+
  geom_point(aes(date, stbs_r), size = 0.3)+
  geom_line(aes(date, bs_r), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.4, fill = "#48cae4")+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )




