# Comparison with all cause mortality
by_cause <- read_rds("output/cod/dts_cause_country_sex_age.rds")
all_cause <- read_rds("Output/annual_young_deaths_all_countries.rds")

cts_lvs <- 
  c('Austria',
    'Netherlands',
    'Latvia',
    'Estonia',
    'Costa Rica')


excl <- c("0d", "1-11m", "1-6d", "7-27d", "tot")

by_cause2 <- 
  by_cause %>% 
  filter(cause == "ALL CAUSES",
         !age %in% excl) %>% 
  mutate(age = age %>% as.double()) %>% 
  select(-List, -cause_cod, -cause) %>% 
  filter(age < 25) %>% 
  rename(dts_css = dts)

by_cause3 <- 
  by_cause2 %>% 
  bind_rows(by_cause2 %>% 
              group_by(year, country, age) %>% 
              summarise(dts_css = sum(dts_css)) %>% 
              ungroup() %>% 
              mutate(sex = "t")) %>% 
  mutate(age = ifelse(age == 1 & !country %in% c("Costa Rica", "Estonia"),
                      0, age)) %>% 
  group_by(year, country, sex, age) %>% 
  summarise(dts_css = sum(dts_css)) %>% 
  ungroup() %>% 
  arrange(year, country, sex, age)

unique(by_cause2$age)

cts_cause <- 
  by_cause2 %>% 
  pull(country) %>% 
  unique()

all_cause2 <- 
  all_cause %>% 
  filter(Country %in% cts_cause) %>% 
  select(country = Country,
         year = Year,
         sex = Sex,
         age = Age,
         dts_tot = Deaths) %>% 
  filter(age < 25)

unique(all_cause2$country)
ages <- 
  all_cause2 %>% 
  select(country, age) %>% 
  unique()

comp <- 
  by_cause3 %>% 
  left_join(all_cause2) %>% 
  mutate(country = factor(country, levels = cts_lvs))


comp %>% 
  filter(sex == "t") %>% 
  gather(dts_css, dts_tot, key = source, value = dts) %>% 
  ggplot()+
  geom_point(aes(age, dts, col = source), alpha = 0.5)+
  scale_y_log10()+
  facet_grid(year~country)+
  scale_color_manual(values = c("#ff006e", "#3a86ff"))+
  theme_bw()+
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        strip.text = element_text(size = 5, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/cod_deaths_comparison_source.png")

# ====




# comparing baselines ====
# ~~~~~~~~~~~~~~~~~~~~~~~~

cod <- 
  read_rds("output/cod/baseline_cause_sex_age.rds")

all <- 
  read_rds("Output/p_scores_excess_raw_data.rds")


cod2 <- 
  cod %>% 
  filter(cause == "ALL CAUSES",
         !age %in% excl) %>% 
  select(year, country, sex, age, dts, bsn) %>% 
  mutate(age = age %>% as.double()) %>% 
  arrange(year, country, sex, age) %>% 
  group_by(year, country, sex) %>% 
  mutate(age_up = lead(age),
         age = case_when(age == 0 & age_up == 1 ~ "Infant",
                         age == 0 & age_up == 5 ~ "0_4",
                         age == 1 & age_up == 5 ~ "1_4",
                         age == 5 ~ "5_9",
                         age == 10 ~ "10_14",
                         age == 15 ~ "15_19",
                         age == 20 ~ "20_24")) %>% 
  rename(dts_cod = dts,
         bsn_cod = bsn) %>% 
  select(-age_up)

unique(cod2$age)


cod_04 <- 
  cod2 %>% 
  filter(age %in% c("Infant", "1_4")) %>% 
  group_by(year, country, sex) %>% 
  summarise(dts_cod = sum(dts_cod),
            bsn_cod = sum(bsn_cod)) %>% 
  ungroup() %>% 
  mutate(age = "0_4")

cod3 <- 
  bind_rows(cod2,
            cod_04) %>% 
  arrange(year, country, sex, age) %>% 
  filter(!age %in% excl)


all2 <- 
  all %>% 
  filter(Country %in% cts_cause) %>% 
  select(country = Country,
         year = Year,
         sex = Sex,
         age = Age,
         dts_tot = Deaths,
         bsn_tot = Baseline)

unique(cod3$age)

comp_bsn <- 
  all2 %>% 
  full_join(cod3) %>% 
  mutate(pscore_tot = dts_tot / bsn_tot,
         pscore_cod = dts_cod / bsn_cod,
         age = factor(age, levels = c("Infant",
                                      "1_4",
                                      "0_4",
                                      "5_9",
                                      "10_14",
                                      "15_19",
                                      "20_24")),
          country = factor(country, levels = cts_lvs))

comp_bsn %>% 
  select(year, country, sex, age, pscore_tot, pscore_cod) %>% 
  # filter(sex == "t") %>% 
  filter(year == 2020) %>% 
  gather(pscore_tot, pscore_cod, key = source, value = pscore) %>% 
  ggplot()+
  geom_point(aes(age, pscore, col = source), alpha = 0.5)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10()+
  facet_grid(sex~country)+
  scale_color_manual(values = c("#ff006e", "#3a86ff"))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5, angle = 60, hjust = 1),
        axis.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        strip.text = element_text(size = 5, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/cod_pscores_comparison_source.png")



comp_bsn %>% 
  select(year, country, sex, age, pscore_tot, pscore_cod) %>% 
  # filter(sex == "t") %>% 
  filter(year == 2020) %>% 
  gather(pscore_tot, pscore_cod, key = source, value = pscore) %>% 
  ggplot()+
  geom_point(aes(pscore, country, col = source), alpha = 0.5)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  # scale_y_log10()+
  scale_x_log10()+
  facet_grid(sex~age)+
  scale_color_manual(values = c("#ff006e", "#3a86ff"))+
  theme_bw()+
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 5),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        strip.text = element_text(size = 7, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/cod_pscores_comparison_source.png")

