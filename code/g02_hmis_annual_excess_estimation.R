rm (list = ls())
source("code/00_functions.R")

hmis <- read_rds("data_inter/hmis_all_countries.rds")


# ~~~~~~~~~~~~~~~~~~~
# preparing data ====
# ~~~~~~~~~~~~~~~~~~~
sbs <- 
  hmis %>% 
  filter(measure %in% c("sbs", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = sbs,
         exposure = bts + sbs) %>% 
  drop_na(outcome, exposure)

neo <- 
  hmis %>% 
  filter(measure %in% c("neo", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = neo,
         exposure = bts) %>% 
  drop_na(outcome, exposure)

inf <- 
  hmis %>% 
  filter(measure %in% c("inf", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = inf,
         exposure = bts) %>% 
  drop_na(outcome, exposure)

chd <- 
  hmis %>% 
  filter(measure == "0_4") %>% 
  mutate(outcome = value)


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# baselines estimation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_sbs <- 
  sbs %>% 
  group_by(country) %>% 
  do(est_baseline_w_exp(chunk = .data)) %>% 
  ungroup()

bsn_neo <- 
  neo %>% 
  group_by(country) %>% 
  do(est_baseline_w_exp(chunk = .data)) %>% 
  ungroup()

bsn_chd <- 
  chd %>% 
  group_by(country) %>% 
  do(est_baseline_wo_exp(chunk = .data)) %>% 
  ungroup()


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# putting all together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
psc <- 
  bind_rows(
    bsn_sbs %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Stillbirths"),
    bsn_neo %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Neonatal"),
    bsn_chd %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Child (0-4)")
    )

write_rds(psc, "data_inter/hmis_annual_baselines.rds")
write_rds(psc, paste0("data_output/preliminary_pscores_hmis_", today(), ".rds"))

