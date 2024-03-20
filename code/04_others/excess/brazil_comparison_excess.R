library(here)
source(here("Code", "00_functions.R"))
library(ggpubr)

# loading excess estimates
# ========================
excess_fetal_region <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_region.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+") %>% 
  select(date, region, p_score, p_score_un) %>% 
  mutate(type = "stillbirths")

excess_fetal_state <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_state.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+") %>% 
  select(date, region, state_iso, p_score, p_score_un) %>% 
  mutate(type = "stillbirths")

excess_all_ages_region <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_region.rds") %>% 
  select(date, region, p_score, p_score_un) %>% 
  mutate(type = "all_age")

excess_all_ages_state <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_state.rds") %>% 
  select(date, region, state_iso, p_score, p_score_un) %>% 
  mutate(type = "all_age")

excess_neo_region <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_region.rds") %>% 
  ungroup() %>% 
  filter(age == "neo") %>% 
  select(date, region, p_score, p_score_un) %>% 
  mutate(type = "neonatal")

excess_neo_state <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_state.rds") %>% 
  ungroup() %>% 
  filter(age == "neo") %>% 
  select(date, region, state_iso, p_score, p_score_un) %>% 
  mutate(type = "neonatal")

db_state <- 
  bind_rows(excess_fetal_state,
            excess_all_ages_state,
            excess_neo_state)

db_region <- 
  bind_rows(excess_fetal_region,
            excess_all_ages_region,
            excess_neo_region)

db_state %>% 
  filter(date >= "2020-01-15" & date <= "2020-12-15") %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, p_score_un, col = type, shape = out), alpha = 1, 
             size = 1)+
  facet_wrap(region~state_iso, ncol = 7)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_x_date(date_breaks = "3 months", date_labels = "%b")+
  scale_shape_manual(values = c(1, 16), guide = NULL)+
  theme_bw()+
  theme(
    legend.position = c(.93,.08),
    axis.text = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7),
    legend.text = element_text(size = 7),
    legend.title = element_blank()
  )
ggsave("Figures/excess/comparison_state_p_scores.png", dpi = 1000, 
       width = 8, height = 4)

db_region %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  filter(date >= "2020-01-15" & date <= "2020-12-15") %>% 
  ggplot()+
  geom_point(aes(date, p_score_un, col = type, shape = out), alpha = 1)+
  facet_grid(~region)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  geom_hline(yintercept = 1, linetype = "dashed", col = "black")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b")+
  scale_y_log10(breaks = c(0.8, 1, 1.25, 1.5, 1.75, 2))+
  scale_shape_manual(values = c(1, 16), guide = NULL)+
  theme_bw()+
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank()
  )
ggsave("Figures/excess/comparison_region_p_scores.png", dpi = 1000, 
       width = 8, height = 4)

# =================

# Regional analysis
# =================

# totals p_score during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excess_fetal_region_total <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_region.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+",
         date >= "2020-03-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, fds - bsn, 0)) %>% 
  group_by(region) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_sbs = (excess + bsn) / bsn) %>% 
  select(region, p_score_sbs)

excess_all_ages_region_total <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_region.rds") %>% 
  ungroup() %>% 
  filter(date >= "2020-03-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  group_by(region) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_dts = (excess + bsn) / bsn) %>% 
  select(region, p_score_dts)

excess_neo_region_total <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_region.rds") %>% 
  ungroup() %>% 
  filter(age == "neo", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  group_by(region) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_neo = (excess + bsn) / bsn) %>% 
  select(region, p_score_neo)

excess_region_total <- 
  excess_all_ages_region_total %>% 
  left_join(excess_fetal_region_total) %>% 
  left_join(excess_neo_region_total)

excess_region_total %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_sbs))

excess_region_total %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_neo))


# monthly p_score during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excess_all_ages_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_region.rds") %>% 
  ungroup() %>% 
  filter(date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  select(date, region, p_score_dts = p_score_un)

excess_sbs_region_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_region.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, fds - bsn, 0)) %>% 
  select(date, region, p_score = p_score_un)

excess_neo_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_region.rds") %>% 
  ungroup() %>% 
  filter(age == "neo", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  select(date, region, p_score = p_score_un)

all_neo <- 
  excess_all_ages_mth %>% 
  left_join(excess_neo_mth) %>% 
  mutate(type = "neonatal",
         corr = cor(excess_neo_mth$p_score, 
                    excess_all_ages_mth$p_score_dts, 
                    method = c("pearson")) %>% round(3))

all_sbt <- 
  excess_all_ages_mth %>% 
  left_join(excess_sbs_region_mth) %>% 
  mutate(type = "stillbirths",
         corr = cor(excess_sbs_region_mth$p_score, 
                    excess_all_ages_mth$p_score_dts, 
                    method = c("pearson")) %>% round(3))

both <- 
  bind_rows(all_neo, 
            all_sbt )

both %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score), alpha = 0.5)+
  geom_text(aes(1.85, 1.27, label = paste0("corr = ", corr)), col = "#184e77")+
  geom_vline(xintercept = 1, linetype = "dashed", col = "#118ab2")+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  scale_x_log10(breaks = seq(1, 2, 0.25))+
  scale_y_log10(breaks = seq(0.8, 1.4, 0.1))+
  facet_grid(type~.)+
  theme_bw()

ggsave("Figures/excess/comparison_correlation_region.png", dpi = 1000, 
       width = 4, height = 4)


cor.test(excess_sbs_region_mth$p_score, 
         excess_all_ages_mth$p_score_dts, 
         method = "pearson")$p.value

cor.test(excess_neo_mth$p_score, 
         excess_all_ages_mth$p_score_dts, 
         method = "pearson")$p.value


# State analysis
# =================

# totals p_score during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excess_all_ages_state_total <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_state.rds") %>% 
  ungroup() %>% 
  filter(date >= "2020-03-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  group_by(state_iso) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_dts = (excess + bsn) / bsn) %>% 
  select(state_iso, p_score_dts)

excess_fetal_state_total <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_state.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+",
         date >= "2020-03-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, fds - bsn, 0)) %>% 
  group_by(state_iso) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_sbs = (excess + bsn) / bsn) %>% 
  select(state_iso, p_score_sbs)

excess_neo_state_total <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_state.rds") %>% 
  ungroup() %>% 
  filter(age == "neo", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  group_by(state_iso) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score_neo = (excess + bsn) / bsn) %>% 
  select(state_iso, p_score_neo)

excess_state_total <- 
  excess_all_ages_state_total %>% 
  left_join(excess_fetal_state_total) %>% 
  left_join(excess_neo_state_total)


excess_state_total %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_sbs))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

excess_state_total %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_neo))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()




# monthly p_score during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excess_all_ages_state_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_all_ages_state.rds") %>% 
  ungroup() %>% 
  filter(date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  select(date, state_iso, p_score_dts = p_score_un)

excess_sbt_state_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_state.rds") %>% 
  ungroup() %>% 
  filter(weeks == "28+", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, fds - bsn, 0)) %>% 
  select(date, state_iso, p_score = p_score_un)

excess_neo_state_mth <- 
  read_rds("Output/excess/brazil_monthly_excess_neonatal_state.rds") %>% 
  ungroup() %>% 
  filter(age == "neo", 
         date >= "2020-03-15" & date <= "2020-12-15") %>% 
  mutate(excess = ifelse(p_score_un != 1, dts - bsn, 0)) %>% 
  select(date, state_iso, p_score = p_score_un)


all_neo <- 
  excess_all_ages_state_mth %>% 
  left_join(excess_neo_state_mth) %>% 
  mutate(type = "neonatal",
         corr = cor(excess_neo_state_mth$p_score, 
                    excess_all_ages_state_mth$p_score_dts, 
                    method = c("pearson")) %>% round(3))

all_sbt <- 
  excess_all_ages_state_mth %>% 
  left_join(excess_sbt_state_mth) %>% 
  mutate(type = "stillbirths",
         corr = cor(excess_sbt_state_mth$p_score, 
                    excess_all_ages_state_mth$p_score_dts, 
                    method = c("pearson")) %>% round(3))

both <- 
  bind_rows(all_neo, 
            all_sbt)

both %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score), alpha = 0.5)+
  geom_text(aes(2.3, 2, label = paste0("corr = ", corr)), col = "#184e77")+
  geom_vline(xintercept = 1, linetype = "dashed", col = "#118ab2")+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  scale_x_log10(breaks = seq(1, 2.2, 0.4))+
  scale_y_log10(breaks = seq(0.6, 2, 0.4))+
  facet_grid(type~.)+
  theme_bw()

ggsave("Figures/excess/comparison_correlation_state.png", dpi = 1000, 
       width = 4, height = 4)


cor.test(excess_sbt_state_mth$p_score, 
         excess_all_ages_state_mth$p_score_dts, 
         method = "pearson")$p.value

cor.test(excess_neo_state_mth$p_score, 
         excess_all_ages_state_mth$p_score_dts, 
         method = "pearson")$p.value

mth_dts_vs_sbs <- 
  excess_fetal_state_mth %>% 
  left_join(excess_all_ages_state_mth) %>% 
  left_join(excess_neo_state_mth)

mth_dts_vs_sbs %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_stb))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

mth_dts_vs_sbs %>% 
  ggplot()+
  geom_point(aes(p_score_dts, p_score_neo))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

cor(mth_dts_vs_sbs$p_score_stb, mth_dts_vs_sbs$p_score_dts, method = c("pearson"))
cor(mth_dts_vs_sbs$p_score_neo, mth_dts_vs_sbs$p_score_dts, method = c("pearson"))
