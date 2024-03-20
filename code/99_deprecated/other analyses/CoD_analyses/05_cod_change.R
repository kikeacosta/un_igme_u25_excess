source("Code/00_functions.r")
# Cause of deaths analysis

cod <- 
  read_rds("output/cod/baseline_cause_sex_age.rds")

cod_lvs <- 
  c('ALL CAUSES', 
    'Communicable, maternal, perinatal and nutritional conditions', 
    'HIV/AIDS', 
    'Diarrhoeal diseases', 
    'Pertussis', 
    'Tetanus', 
    'Measles', 
    'Meningitis/encephalitis', 
    'Malaria', 
    'Acute lower respiratory infections', 
    'Prematurity', 
    'All perinatal causes', 
    'Birth asphyxia and birth trauma', 
    'Sepsis and other infectious conditions of the newborn', 
    'Other Group 1', 
    'Noncommunicable diseases', 
    'Congenital anomalies',
    'Chronic obstructive pulmonary disease', 
    'Other noncommunicable diseases', 
    'Injuries', 
    'Ill-defined causes')
# 

cts_lvs <- 
  c('Austria',
    'Netherlands',
    'Latvia',
    'Estonia',
    'Costa Rica')


# excluding causes with too low mortality
cod2 <- 
  cod %>% 
  group_by(country, cause_cod, cause, sex, age, year) %>% 
  filter(sum(dts) > 5 & dts_frc >= 0.05) %>%
  ungroup() %>% 
  mutate(is_exc = case_when(year == 2020 & dts <= lp ~ "negative",
                            year == 2020 & dts >= up ~ "positive",
                            TRUE ~ "none"),
         pscore = dts / bsn,
         pscore_un = ifelse(is_exc != "none", pscore, 1),
         cause = factor(cause, levels = cod_lvs),
         country = factor(country, levels = cts_lvs),
         y2 = year -2000,
         dts_r = dts/pop,
         bsn_r = bsn/pop,
         lp_r = lp/pop,
         up_r = up/pop)

unique(cod2$age)
unique(cod2$cause)
unique(cod2$country)

c <- c("Netherlands", "Austria", "Costa Rica")
c <- unique(cod2$country)
a <- "0"
s <- "t"

cod2 %>% 
  filter(country %in% c,
         age == a,
         sex == "t") %>% 
  ggplot()+
  geom_ribbon(aes(y2, ymin = lp_r, ymax = up_r), 
              fill = "#3f37c9", alpha = 0.2)+
  geom_line(aes(y2, dts_r))+
  geom_point(aes(y2, dts_r, col = is_exc))+
  geom_line(aes(y2, bsn_r), col = "#3f37c9")+
  geom_vline(xintercept = 19.5, linetype = "dashed", col = "black")+
  scale_color_manual(values = c("#4361ee", "black", "#f72585"))+
  facet_grid(cause~country, scales = "free")+
  # labs(title = paste0(c, "_", a))+
  theme_bw()+
  theme(strip.background = element_rect(fill = "transparent"),
        strip.text.y = element_text(angle = 0),
        legend.position = "none")
ggsave(paste0("figures/cod/cod_baseline_fitting_example", a, ".png"),
       w = 10,
       h = 6)

# ====

age_lvs <- c("0d", "1-6d", "7-27d", "1-11m", "0", "1", "5", "10", "15", "20")


cod2020 <- 
  cod2 %>% 
  filter(year == 2020) %>% 
  mutate(age = factor(age, levels = age_lvs))

test <- 
  cod2020 %>% 
  filter(country == c,
         sex == "t")

my_breaks = c(0.1, 0.25, 0.5, 0.8, 1, 1.5, 2, 5, 10)

test <- 
  cod2020 %>% 
  mutate(
    pscore_ad = case_when(
      pscore_un <= -3 ~ -3,
      pscore_un >= 3 ~ 3,
      TRUE ~ pscore_un
    )
  )

cod2020 %>% 
  mutate(
    pscore_un = case_when(
      pscore_un <= -3 ~ -3,
      pscore_un >= 3 ~ 3,
      TRUE ~ pscore_un
      )
    ) %>% 
  ggplot(aes(factor(age), cause, z = pscore_un))+
  geom_tile(aes(fill = pscore_un))+
  scale_fill_gradient2(low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0, 
    trans = "log",
    breaks = my_breaks)+
  facet_grid(country~sex)+
  theme_bw()+
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        strip.text = element_text(size = 8, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/heatmap_cod_changes.png",
       w = 6,
       h = 6)


cod2020 %>% 
  mutate(
    pscore_un = case_when(
      pscore_un <= -3 ~ -3,
      pscore_un >= 3 ~ 3,
      TRUE ~ pscore_un
    )
  ) %>% 
  ggplot(aes(factor(country), cause, z = pscore_un))+
  geom_tile(aes(fill = pscore_un))+
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0, 
                       trans = "log",
                       breaks = my_breaks)+
  facet_grid(age~sex)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 7, angle = 60, hjust = 1),
        axis.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        strip.text = element_text(size = 8, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/heatmap_cod_changes_cntry.png",
       w = 6,
       h = 6)




inf_ages <- c("0d", "1-6d", "7-27d", "1-11m", "0")
chd_ages <- c("0", "1", "5", "10", "15", "20")

tx <- 7
cod2020 %>% 
  filter(age %in% inf_ages,
         country != "Costa Rica",
         sex == "t") %>% 
  mutate(
    pscore_un = case_when(
      pscore_un <= -3 ~ -3,
      pscore_un >= 3 ~ 3,
      TRUE ~ pscore_un
    )
  ) %>% 
  ggplot(aes(factor(country), cause, z = pscore_un))+
  geom_tile(aes(fill = pscore_un))+
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0, 
                       trans = "log",
                       breaks = my_breaks)+
  facet_grid(~age)+
  theme_bw()+
  theme(axis.text.y = element_text(size = tx),
        axis.text.x = element_text(size = tx, angle = 60, hjust = 1),
        axis.title = element_text(size = tx),
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/heatmap_cod_changes_cntry_infant.png",
       w = 8,
       h = 4)

tx <- 7
cod2020 %>% 
  filter(age %in% chd_ages,
         # country != "Costa Rica",
         sex == "t") %>% 
  mutate(
    pscore_un = case_when(
      pscore_un <= -3 ~ -3,
      pscore_un >= 3 ~ 3,
      TRUE ~ pscore_un
    )
  ) %>% 
  ggplot(aes(factor(country), cause, z = pscore_un))+
  geom_tile(aes(fill = pscore_un))+
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0, 
                       trans = "log",
                       breaks = my_breaks)+
  facet_grid(~age)+
  theme_bw()+
  theme(axis.text.y = element_text(size = tx),
        axis.text.x = element_text(size = tx, angle = 60, hjust = 1),
        axis.title = element_text(size = tx),
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx, 
                                  margin = margin(t = 0, b = 0))
  )

ggsave("figures/cod/heatmap_cod_changes_cntry_child_young.png",
       w = 8,
       h = 4)

