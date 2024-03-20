rm (list = ls())
source("Code/causes/00_functions_cause_analyses.R")

dts <- 
  read_rds("Output/deaths_by_cause_jh_db_and_exposures.rds")


# removing covid deaths from fitting
covid_dts <- 
  dts %>% 
  filter(cod == "covid") %>% 
  select(country, age, age_type, year, cod, dts, exposure) %>% 
  mutate(bsn = 0,
         ll = 0, 
         ul = 0)

dts2 <- 
  dts %>% 
  filter(cod != "covid")

# predicting baselines ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
unique(dts$cod) %>% sort()

# predicting 2020 all-cause mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_all <- 
  dts2 %>% 
  group_by(country, year, age_type, age, t, w, exposure) %>% 
  summarise(dts = sum(dts),
            exposure = unique(exposure)) %>% 
  ungroup() %>% 
  mutate(cod = "All_cause") %>% 
  group_by(country, age) %>% 
  do(fit_annual(chunk = .)) %>% 
  ungroup() 

bsn_all %>% 
  filter(country == "Costa Rica",
         age == "0") %>% 
  ggplot()+
  geom_point(aes(year, dts))+
  geom_line(aes(year, bsn))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(aes(year, ymin = ll, ymax = ul), alpha = 0.3)+
  theme_bw()


# cts <- unique(bsn_all$country)
# for(c in cts){
#   
#   temp1 <- 
#     bsn_all %>% 
#     filter(country == c)
#   
#   ags <- unique(temp$age)
#   
#   for(a in ags){
#     temp2 <-
#       temp1 %>%
#       filter(country == c,
#              age == a)
# 
#     test <- fit_annual(temp)
# 
#   }
# }



# predicting 2020 mortality by cause ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_causes <- 
  dts2 %>% 
  group_by(country, age, cod) %>% 
  # filter(!any(dts == 0)) %>% 
  do(fit_annual(chunk = .)) %>% 
  ungroup() 

bsn_causes %>% 
  filter(country == "Germany",
         age == "0",
         cod == "c150_residual_gr2") %>% 
  ggplot()+
  geom_point(aes(year, dts))+
  geom_line(aes(year, bsn))+
  # geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(aes(year, ymin = ll, ymax = ul), alpha = 0.3)+
  theme_bw()

unique(bsn_causes$age)
# adding covid deaths 
bsn_causes2 <- 
  bsn_causes %>% 
  select(-t, -w, -age_exp) %>% 
  bind_rows(covid_dts)

# compare predictions, sum of cause-specific prediction and all cause
bsn_cause_sum <- 
  bsn_causes2 %>% 
  filter(year == 2020) %>%
  group_by(country, age) %>% 
  summarise(dts_css = sum(dts),
            bsn_css = sum(bsn)) %>% 
  ungroup()

pred_comp <- 
  bsn_all %>% 
  filter(year == 2020) %>% 
  bind_rows(covid_dts) %>% 
  group_by(country, age_type, age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  left_join(bsn_cause_sum) %>% 
  mutate(diff = dts - dts_css) %>% 
  mutate(ratio_all_sum = bsn / bsn_css) %>% 
  filter(age_type == "young")

pred_inf_comp <- 
  bsn_all %>% 
  filter(year == 2020) %>% 
  bind_rows(covid_dts) %>% 
  group_by(country, age_type, age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  left_join(bsn_cause_sum) %>% 
  mutate(diff = dts - dts_css) %>% 
  mutate(ratio_all_sum = bsn / bsn_css) %>% 
  filter(age_type == "infant") %>% 
  group_by(country) %>% 
  summarise(dts = sum(dts),
            dts_css = sum(dts_css),
            bsn = sum(bsn),
            bsn_css = sum(bsn_css)) %>% 
  ungroup() %>% 
  mutate(ratio_all_sum = bsn / bsn_css)

cts_excl <- 
  pred_inf_comp %>% 
  filter(dts < 100) %>% 
  pull(country)


# groups causes
gr1 <- c("c020", "c030", "c040", "c050", "c060", "c070", "c071", "c080", 
         "c090", "c091", "c100", "c110", "c120_residual_gr1")
gr2 <- c("c140", "c141", "c150_residual_gr2")
gr3 <- "c160"
gr4 <- "c900"
gr5 <- "covid"


lvs_causes <- 
  conts %>% 
  select(cod, cause) %>% 
  unique() %>% 
  arrange(cod) %>% 
  pull(cause) %>% 
  unique()


conts <- 
  bsn_causes2 %>% 
  filter(year == 2020) %>% 
  select(-exposure, -ll, -ul) %>% 
  mutate(exc = dts - bsn) %>%
  group_by(country, age) %>% 
  mutate(pscore_tot = sum(dts) / sum(bsn),
         exc_tot = sum(exc),
         exc_neg = sum(ifelse(exc < 0, exc, 0)),
         exc_pos = sum(ifelse(exc > 0, exc, 0))) %>% 
  ungroup() %>% 
  mutate(cont = exc / ifelse(exc < 0, -exc_neg, exc_pos),
         cont = ifelse(is.na(cont), 0, cont)) %>% 
  # left_join(chapters) %>% 
  mutate(country2 = paste0(country, " (", round(pscore_tot, 2), ")"),
         age = factor(age, levels = c("neo", "post_neo",
                                      "0", "1", "5", "10", "15", '20')),
         g_jh = case_when(cod %in% gr1 ~ "gr1",
                          cod %in% gr2 ~ "gr2",
                          cod %in% gr3 ~ "gr3",
                          cod %in% gr4 ~ "gr4",
                          cod %in% gr5 ~ "gr5")) %>% 
  filter(!country %in% c("United Arab Emirates")) %>% 
  mutate(cause = case_when(cod == 'c020' ~ 'HIV/AIDS',
                           cod == 'c030' ~ 'Diarrhoeal diseases',
                           cod == 'c040' ~ 'Pertussis',
                           cod == 'c050' ~ 'Tetanus',
                           cod == 'c060' ~ 'Measles',
                           cod == 'c070' ~ 'Meningitis/encephalitis',
                           cod == 'c071' ~ 'Malaria',
                           cod == 'c080' ~ 'Acute lower respiratory inf',
                           cod == 'c090' ~ 'Prematurity',
                           cod == 'c091' ~ 'All perinatal causes',
                           cod == 'c100' ~ 'Birth asphyxia and birth trauma',
                           cod == 'c110' ~ 'Sepsis and oth infec newborn',
                           cod == 'c120_residual_gr1' ~ 'Other Group 1',
                           cod == 'c130' ~ 'Noncommunicable diseases',
                           cod == 'c140' ~ 'Congenital anomalies',
                           cod == 'c141' ~ 'COPD',
                           cod == 'c150_residual_gr2' ~ 'Residual noncom dis',
                           cod == 'c160' ~ 'Injuries',
                           cod == 'c900' ~ 'Ill-defined causes',
                           cod == 'covid' ~ 'Covid-19'))

conts2 <- 
  conts %>% 
  mutate(cause = factor(cause, levels = lvs_causes))

unique(conts$age)
unique(conts$cod)
unique(conts$country)
unique(conts$cause)

library(scales)
conts2 %>%
  filter(age == "0",
         !country %in% cts_excl) %>% 
  ggplot()+
  geom_tile(aes(cause, country, fill = cont, col = g_jh))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  # facet_wrap(~chapter, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1))


conts2 %>%
  filter(age_type == "infant",
         !country %in% cts_excl,
         country != "England and Wales") %>% 
  ggplot()+
  geom_tile(aes(cause, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_wrap(~age, scales = "free_y")+
  # facet_nested(~age+g_jh, scales = "free", space = "free")+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(legend.position = "none",
        # panel.spacing.x = unit(0,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/who_jh_causes_ages_infant.png"), 
       dpi = 600, width = 15, height = 8)

conts_inf <- 
  conts2 %>% 
  filter(age_type == "infant",
         !country %in% cts_excl,
         country != "England and Wales") %>% 
  group_by(country, country2, age, pscore_tot) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            exc = sum(exc)) %>% 
  group_by(country) %>% 
  mutate(cont = exc/max(abs(exc))) %>% 
  ungroup()

conts2 %>%
  filter(age_type == "young",
         # age %in% c("0", "1", "5"),
         !country %in% cts_excl,
         country != "England and Wales") %>% 
  ggplot()+
  geom_tile(aes(cause, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_wrap(~age, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/who_jh_causes_ages_young.png"), 
       dpi = 600, width = 15, height = 8)

