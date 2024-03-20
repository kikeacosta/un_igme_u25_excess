rm (list = ls())
source("Code/00_functions.R")

all <- read_rds("data_inter/neonatal_and_stillbirths_vital_reg.rds")


income_levels <- read_xlsx("Data/world_bank/CLASS.xlsx") %>% 
  drop_na(Region) %>% 
  select(code = 2,
         Region = 3,
         income = 4) %>% 
  mutate(income = case_when(income == "High income" ~ "High",
                            income == "Low income" ~ "Low",
                            income == "Upper middle income" ~ "Upper-mid",
                            income == "Lower middle income" ~ "Lower-mid",
                            TRUE ~ income))

unique(income_levels$income)
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low")


# summary of available data
all %>% 
  filter(year >= 2020,
         measure %in% c("neo", "sbs")) %>% 
  select(country, year, measure) %>% 
  unique() %>%
  mutate(n = 1) %>% 
  spread(year, n) %>% 
  replace_na(list(`2020` = 0, `2021` = 0)) %>% 
  group_by(measure) %>% 
  summarise(`2020` = sum(`2020`),
            `2021` = sum(`2021`))

cts_cmm_20 <- 
  all %>% 
  filter(year %in% 2015:2020) %>% 
  select(country, year, measure, value) %>% 
  spread(year, value) %>% 
  drop_na() %>% 
  select(country, measure) %>% 
  mutate(keep = 1)

cts_cmm_20 <- 
  all %>% 
  filter(year %in% 2015:2021) %>% 
  select(country, year, measure, value) %>% 
  spread(year, value) %>% 
  drop_na() %>% 
  select(country, measure) %>% 
  mutate(keep = 1)

all2 <- 
  all %>% 
  filter(measure %in% c("neo", "sbs"),
         year %in% 2015:2021) %>% 
  left_join(cts_cmm_20) %>% 
  filter(keep == 1) %>% 
  left_join(income_levels, by = "code")

all3 <- 
  all2 %>% 
  group_by(income, measure, year) %>% 
  summarise(value = sum(value),
            exposure = sum(exposure)) %>% 
  ungroup()
  
all4 <- 
  all3 %>% 
  group_by(measure, year) %>% 
  summarise(value = sum(value),
            exposure = sum(exposure)) %>% 
  ungroup() %>% 
  mutate(income = "Total") %>% 
  bind_rows(all3)


fit_sbs_neo <- function(chunk){
  
  model <- glm(value ~ t + offset(log(exposure)),
               data = chunk,
               weights = w,
               family = "quasipoisson")
  
  test <- try(pred <- predict(model, 
                              se.fit = TRUE,
                              type = "response"))
  
  try(out <- 
        chunk %>%
        mutate(bsn = pred$fit,
               lp = (pred$fit - 1.96 * pred$se.fit),
               up = (pred$fit + 1.96 * pred$se.fit)))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             lp = NA,
             up = NA)
    
  }
  
  return(out)
}

bsn <- 
  all4 %>% 
  filter(measure %in% c('neo', 'sbs')) %>% 
  group_by(income, measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(year < 2020, 1, 0)) %>% 
  do(fit_sbs_neo(chunk = .data)) %>% 
  ungroup()


bsn %>% 
  mutate(rate = value / exposure,
         bsn_r = bsn / exposure) %>% 
  ggplot()+
  geom_point(aes(year, rate))+
  geom_line(aes(year, bsn_r))+
  facet_wrap(measure ~income)+
  expand_limits(y = 0)

income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low")

bsn2 <- 
  bsn %>% 
  filter(year == 2020) %>% 
  mutate(psc = value / bsn,
         psc_lp = value / up,
         psc_up = value / lp) %>%
  mutate(excess = case_when(psc_up < 1 ~ "Negative",
                            psc_lp > 1 ~ "Positive",
                            TRUE ~ "No-excess"),
         income = factor(income, levels = income_lvs))

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")


bks <-  c(0.6, 0.8, 0.9, 1, 1.1, 1.2, 1.5, 2, 2.5)
lbs <- paste0((bks - 1)*100, "%")

bsn2 %>% 
  mutate(measure = case_when(measure == "sbs" ~ "Stillbirths",
                             measure == "neo" ~ "Neonatal"),
         measure = factor(measure, levels = c("Stillbirths","Neonatal"))) %>% 
  ggplot(aes(psc, income))+
  geom_point(aes(psc, income, 
                 col = excess),
             alpha = 0.8)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_text(aes(psc, income, label = round((psc - 1)*100, 1)),
            vjust = 0.5, hjust = -0.5, size = 3.5)+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(1/1.25, 1.25)
                )+
  scale_y_discrete(limits=rev)+
  facet_nested(income ~ measure, scales = "free", space = "free_y")+
  labs(x = "p-score")+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.position = "none",
        legend.title = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank())

ggsave(paste0("Figures/last version/global_excess_neo.png"), 
       dpi = 600, width = 6, height = 2.5)





