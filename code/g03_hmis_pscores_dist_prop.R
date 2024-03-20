rm (list = ls())
source("code/00_functions.R")

# loading data ====
# ~~~~~~~~~~~~~~~~~

# on p-score estimates
bsn <- read_rds("data_inter/hmis_annual_baselines.rds")

# data on births
hmis <- read_rds("data_inter/hmis_all_countries.rds")

# on time series
no_complete <- 
  hmis %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(country, measure, year, month) %>% 
  unique() %>% 
  group_by(measure, country, year) %>% 
  summarise(mts = n()) %>% 
  ungroup() %>% 
  filter(measure %in% c("0_4", "neo", "neo_r", "sbs", "sbs_r"),
         year %in% 2020:2022,
         mts < 12) %>% 
  mutate(measure = case_when(measure == "0_4" ~ "Child (0-4)",
                             measure %in% c("neo", "neo_r") ~ "Neonatal",
                             measure %in% c("sbs", "sbs_r") ~ "Stillbirths")) %>% 
  select(-mts)

# births
bts <- 
  hmis %>% 
  filter(measure == "bts") %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year) %>% 
  summarise(bts = sum(value),
            pers = n()) %>% 
  ungroup() %>% 
  mutate(bts = bts * (12/pers)) %>% 
  select(-pers)


# on population
ctrs <- unique(bsn$country) %>% as.character()
# exposures for child mortality (ages 0-4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_f <- 
  read_xlsx("data_input/WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx",
            skip = 16) %>% 
  select(3, 11:112) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(country %in% ctrs, 
         year >= 2010) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "f") %>% 
  arrange(country, age) %>% 
  filter(age %in% 0:4) %>% 
  group_by(country, year, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

pop_m <- 
  read_xlsx("data_input/WPP2022_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx",
            skip = 16) %>% 
  select(3, 11:112) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(country %in% ctrs, 
         year >= 2010) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "m") %>% 
  arrange(country, age) %>% 
  filter(age %in% 0:4) %>% 
  group_by(country, year, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

pop <- 
  bind_rows(pop_f, pop_m) %>%
  group_by(country, year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() 


# putting together baselines and exposures 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
psc <- 
  bsn %>% 
  filter(
    # year %in% 2020:2022,
    measure != "Infant"
    ) %>% 
  # anti_join(no_complete) %>% 
  mutate(psc = outcome / bsn,
         psc_lp = outcome / up,
         psc_up = outcome / lp,
         exc = case_when(psc_lp > 1 ~ "Excess",
                         psc_up < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, levels = c("Deficit", "No-change", "Excess")),
         out = ifelse(exc == "No-change", 0, 1),
         measure = factor(measure,
                          levels = c("Stillbirths",
                                     "Neonatal",
                                     # "Infant",
                                     "Child (0-4)"))) %>% 
  left_join(bts) %>% 
  left_join(pop) %>% 
  mutate(pop = ifelse(measure == "Child (0-4)", pop, bts)) %>% 
  rename(age = measure) %>% 
  # adding last year population available were missing values
  group_by(country, age) %>% 
  arrange(year) %>% 
  mutate(pop = ifelse(is.na(pop), lag(pop), pop)) %>% 
  ungroup() %>% 
  select(year, country, age, outcome, bts, pop, bsn, everything())
  
write_csv(psc, paste0("data_output/preliminary_hmis_pscores_", today(), ".csv"))
write_rds(psc, paste0("data_output/preliminary_hmis_pscores_", today(), ".rds"))

# ~~~~~~~~~~~~~~~~~~~~~~
# Weighted averages ====
# ~~~~~~~~~~~~~~~~~~~~~~

# weighted average P-scores in all populations
wgths <- 
  psc %>% 
  filter(year >= 2020) %>% 
  group_by(year, age) %>% 
  mutate(w = pop / mean(pop)) %>% 
  ungroup()

# install.packages("drf")
# library(drf)
chunk <- 
  wgths %>% 
  filter(age == "Neonatal",
         year == 2020)


compute_qs <- function(chunk){
  x <- chunk$psc
  w <- chunk$w
  
  t <- 
    weighted.quantile(x, w, probs=seq(0,1,0.25), na.rm = TRUE) %>% 
    as.double()
  
  chunk %>% 
    group_by() %>% 
    summarise(q0 = t[1],
              q25 = t[2],
              q50 = t[3],
              q75 = t[4],
              q100 = t[5],
              mean = weighted.mean(x, w, na.rm = FALSE))
}

tot_stats <- 
  wgths %>% 
  group_by(age, year) %>% 
  do(compute_qs(chunk = .data)) %>% 
  ungroup()

inc_stats <- 
  wgths %>% 
  group_by(age, year) %>% 
  do(compute_qs(chunk = .data)) %>% 
  ungroup()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HMIS plots of distributions and proportions ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


cols <- 
  c("Excess" = "#b7094c",
    "No-change" = "#5c4d7d",
    "Deficit" = "#0091ad")

tx <- 8

lms <- c(0.4, 2.2)
bks <- c(0.1, 0.2, 0.5, 0.7, 1, 1.5, 2, 5, 10)
lbs <- paste0((bks - 1)*100, "%")

pl_dist <-
  psc %>% 
  filter(
    year %in% 2020:2022
  ) %>% 
  ggplot(aes(x = psc, y = age)) +
  geom_violin(trim = FALSE, fill = "white", linewidth = 0.3)+
  geom_jitter(aes(col = exc), 
              size = 1.5, alpha = 0.5, 
              height = 0.18,
              width = 0)+
  # geom_point(data = tot_stats,
  #            aes(q50, age),
  #            size = 3,
  #            col = "black", alpha = 0.6)+
  geom_segment(data = tot_stats,
               aes(x = q25, y = age, xend = q75, yend = age))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid( ~ year, scales = "free", space = "free_y")+
  labs(x = "P-score", y = "Age", col = "Change", fill = "Change")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.3, "lines"))


# Summary
# ~~~~~~~~
psc_summ <- 
  psc %>% 
  filter(
    year %in% 2020:2022
  ) %>% 
  select(year, country, age, exc) %>% 
  group_by(year, age) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  # gather(exc, key = type, value = is_excess) %>% 
  group_by(year, age, N_cts, exc) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         exc = factor(exc, 
                      levels = c("Excess", "No-change", "Deficit")))

cols <- 
  c("Excess" = "#b7094c",
    "No-change" = "#5c4d7d",
    "Deficit" = "#0091ad")

# stillbirths and neonatal together
unique(psc_summ$age)

pl_prop <-
  psc_summ %>% 
  mutate(year = factor(year, levels = c("2020", "2021"))) %>% 
  ggplot(aes(x = age, y = Prop))+
  geom_bar(aes(fill = exc),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = exc), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.5)+
  facet_nested( ~ year, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE, 
                                         direction = "horizontal"))+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "25", "50", "75", "100"))+
  coord_flip(expand = 0)+
  labs(fill = "Change",
       y = 'Proportion (%)', x = "Age")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

plot_grid(pl_dist, pl_prop, 
          labels = c("A", "B"),
          label_size = 14,
          rel_heights = c(1, 1.2),
          ncol = 1)

ggsave("figures/manuscript/fig03_hmis_pscores.png", 
       dpi = 700,
       width = 8, height = 3.5)


