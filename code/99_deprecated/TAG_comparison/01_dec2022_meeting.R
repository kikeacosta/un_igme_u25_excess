rm (list = ls())
source("Code/00_functions.R")
library(readxl)

# extracted from:
# https://msemburi.shinyapps.io/excessvis/
db <- 
  read_csv("Data/who_tag/Excess mortality associated with COVID-19 (April 6, 2022).csv")

unique(db$measure)
unique(db$source)

db2 <- 
  db %>% 
  # filter(measure == "excess") %>% 
  mutate(source = str_trim(source),
         year = case_when(str_detect(source, "2019") ~ 2019,
                          str_detect(source, "2020") ~ 2020,
                          str_detect(source, "2021") ~ 2021),
         source2 = case_when(str_detect(source, "Final") ~ "Final",
                             str_detect(source, "Predicted") ~ "Predicted",
                             str_detect(source, "Expected") ~ "Expected",
                             str_detect(source, "GHE") ~ "GHE",
                             TRUE ~ "oth"))


db2 %>% 
  filter(year == 2020,
         measure == "deaths") %>% 
  ggplot()+
  geom_point(aes(age, val, col = source2))+
  # scale_y_log10()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_grid(~sex)+
  theme_bw()


exc <- 
  db2 %>% 
  select(source2, year, sex, age, measure, val) %>% 
  filter(source2 %in% c("Final", "Expected"),
         measure == "excess")

db3 <- 
  db2 %>% 
  select(source2, year, sex, age, measure, val, lwr, uppr) %>% 
  filter(source2 %in% c("Final", "Expected"),
         measure == "deaths") %>% 
  group_by(source2, year, age, measure) %>% 
  summarise(val = sum(val),
            lwr = sum(lwr),
            uppr = sum(uppr)) %>% 
  ungroup() %>% 
  mutate(sex = "Total") %>% 
  bind_rows(db2 %>% 
              select(source2, year, sex, age, measure, val, lwr, uppr) %>% 
              filter(source2 %in% c("Final", "Expected"),
                     measure == "deaths")) 

expec <- 
  db3 %>% 
  filter(source2 == "Expected") %>% 
  select(everything(), -lwr, -uppr, -source2, -measure, expect = val)

psc <- 
  db3 %>% 
  filter(!source2 == "Expected") %>% 
  left_join(expec) %>% 
  mutate(psc = val / expect,
         lp = lwr / expect,
         up = uppr / expect) %>% 
  select(year, sex, age, psc, lp, up) %>% 
  mutate(source = "who")

psc %>% 
  ggplot()+
  geom_point(aes(age, psc, col = sex))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(~year)+
  theme_bw()



# loading overall p-scores
# ~~~~~~~~~~~~~~~~~~~~~~~~
psc_own <- 
  read_rds("data_inter/overall_pscores.rds")

unique(psc_own$Age)

psc_own2 <- 
  psc_own %>% 
  filter(Income == "Total",
         Age %in% c("0-4", "5-9", "10-14", "15-19", "20-24"),
         (fit == "15_20" & Year == "2020") | (fit == "15_21" & Year == "2021")) %>% 
  select(sex = Sex, age = Age, year = Year, psc, lp, up) %>% 
  mutate(age = str_sub(age, 1, 2),
         age = str_replace(age, "-", ""),
         age = age %>% as.numeric(),
         year = year %>% as.numeric(),
         source = "unicef",
         sex = case_when(sex == "f" ~ "Female",
                         sex == "m" ~ "Male",
                         sex == "t" ~ "Total"))

psc_all <- 
  psc_own2 %>% 
  bind_rows(psc)


psc_all %>% 
  ggplot()+
  geom_point(aes(age, psc, col = sex, shape = source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(~year)+
  theme_bw()

cols <- c("#ef476f", "#118ab2")
tx <- 12
psc_all %>% 
  filter(age <= 24) %>% 
  ggplot()+
  geom_point(aes(psc, age, col = source))+
  # geom_linerange(aes(xmin = lp, xmax = up, y = age, 
  #                    col = source))+
  geom_errorbar(aes(xmin = lp, xmax = up, y = age, col = source),
                linewidth = 0.6, width = 0.6, alpha = 0.8)+
  scale_x_log10(limits = c(0.6, 1.5), breaks = c(0.8, 0.9, 1, 1.1, 1.2))+
  scale_y_continuous(breaks = seq(0, 90, 5))+
  scale_color_manual(values = cols)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(sex~year)+
  theme_bw()+
  theme(strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank())

ggsave("Figures/tag_who/comparison_pscores_sex.png",
       w = 8,
       h = 4)

psc_all %>% 
  filter(age <= 24,
         sex == "Total") %>% 
  ggplot()+
  geom_point(aes(psc, age, col = source))+
  # geom_linerange(aes(xmin = lp, xmax = up, y = age, 
  #                    col = source))+
  geom_errorbar(aes(xmin = lp, xmax = up, y = age, col = source),
                linewidth = 0.6, width = 0.6, alpha = 0.8)+
  scale_x_log10(limits = c(0.7, 1.3), breaks = c(0.8, 0.9, 1, 1.1, 1.2))+
  scale_y_continuous(breaks = seq(0, 90, 5))+
  scale_color_manual(values = cols)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(sex~year)+
  theme_bw()+
  theme(strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank())

ggsave("Figures/tag_who/comparison_pscores_tot.png",
       w = 6,
       h = 2)
