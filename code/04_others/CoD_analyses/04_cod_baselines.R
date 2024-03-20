source("Code/00_functions.r")
# Cause of deaths analysis

dts2 <- 
  read_rds("output/cod/cod_deaths_pop_country_cause_sex_age.rds")

# focus on ages <25

unique(dts2$sex)

# including total sex
dts3 <- 
  dts2 %>% 
  group_by(year, country, cause_cod, cause, age) %>% 
  summarise(dts = sum(dts),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(sex = "t") %>% 
  bind_rows(dts2)


# function for fitting the baseline mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

est_baseline <- function(chunk){
  chunk2 <- 
    chunk %>% 
    mutate(w = ifelse(year == 2020, 0, 1))
  test_fit <- 
    try(
      res <- 
        predict(glm(dts ~ year + offset(log(pop)), 
                    weights = w,
                    family = "quasipoisson",
                    data = chunk2), 
                newdata = chunk2,
                type = "response", 
                se.fit = TRUE)
    )
  try(
    chunk3 <- 
      chunk2 %>% 
      bind_cols(tibble(bsn = res$fit,
                       se = res$se.fit))
  )
  if(class(test_fit) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      bind_cols(tibble(bsn = NA,
                       se = NA))
  }
  return(chunk3)
}

bsns <- 
  dts3 %>% 
  mutate(dts = dts + 1) %>% 
  group_by(country, cause_cod, cause, sex, age) %>% 
  do(est_baseline(chunk = .data)) %>% 
  ungroup()

bsns2 <- 
  bsns %>% 
  mutate(dts = round(dts - 1, 2),
         bsn = round(ifelse(bsn - 1 >= 0, bsn - 1, 0), 2),
         lp = bsn - 1.96 * se,
         up = bsn + 1.96 * se)

all_cause <- 
  bsns2 %>% 
  filter(cause == "ALL CAUSES") %>% 
  select(year, country, sex, age, 
         all_dts = dts,
         all_bsn = bsn)

bsns3 <- 
  bsns2 %>% 
  left_join(all_cause) %>% 
  mutate(dts_frc = dts / all_dts,
         bsn_frc = bsn / all_bsn)
  

write_rds(bsns3, "output/cod/baseline_cause_sex_age.rds")


