library(here)
source(here("Code", "00_functions.R"))

db <- read_rds("Output/smoothed_mx_all_countries.rds")

db2 <- 
  db %>% 
  group_by(Country, Code, Year, Sex, type) %>% 
  mutate(age_mid = (Age + lead(Age) - 1) / 2,
         age_mid = ifelse(is.na(age_mid), (Age + 105)/2, age_mid)) %>% 
  ungroup() %>% 
  filter(Year > 2015)


unique(db2$Country)

cts_years <- 
  db2 %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Country) %>% 
  summarise(ymin = min(Year),
            ymax = max(Year),
            pers = n()) %>% 
  arrange(pers)

table(cts_years$Country)






# copy paste in Excel
write.excel(cts_years)

# Mexico analysis
# ~~~~~~~~~~~~~~~
# Sexes are inverted in 2020

mx <- 
  db2 %>% 
  filter(Country == "Mexico") %>% 
  mutate(Sex = case_when(Year == 2020 & Sex == "m" ~ "f",
                         Year == 2020 & Sex == "f" ~ "m",
                         TRUE ~ Sex))

mx %>% 
  filter(Sex == "m",
         type == "mx_pclm_exp") %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Year), alpha = 0.5)+
  scale_y_log10()

mx %>% 
  filter(Sex == "f",
         type == "mx_pclm_exp") %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Year), alpha = 0.5)+
  scale_y_log10()

mx %>% 
  filter(Year == 2020,
         type == "observed") %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Sex), alpha = 0.5)+
  scale_y_log10()

db3 <- 
  db2 %>% 
  filter(Country != "Mexico") %>% 
  bind_rows(mx)

db_avs <- 
  db3 %>% 
  filter(type == "mx_pclm_par",
         Year < 2020) %>% 
  group_by(Country, Code, Age, age_mid, Sex, type) %>% 
  summarise(mx = mean(mx)) %>% 
  ungroup() %>% 
  mutate(Year = "Av")

db4 <- 
  db3 %>% 
  bind_rows(db_avs)

# plot all differences
# ~~~~~~~~~~~~~~~~~~~~

c <- "Colombia"
y <- "2020"
cts <- c("Mexico", "Brazil", "Peru", "Chile", "Colombia")
  
db_sm <- 
  db3 %>% 
  filter(type != "observed",
         Country %in% cts,
         Year == y)

db_ob <- 
  db3 %>% 
  filter(type == "observed",
         Country %in% cts,
         Year == y)

ggplot()+
  geom_line(data = db_sm, aes(age_mid, mx, col = type), alpha = 0.8)+
  geom_point(data = db_ob, aes(age_mid, mx), col = "black", alpha = 0.5, size = 0.5)+
  facet_grid(Country ~ Sex)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  labs(title = paste0("mx age structure in ", y))+
  theme_bw()

ggsave(paste0("Figures/01_example_age_pattern_smoothing_", y, ".png"))

cols <- c(rep("black", 4), "red", "blue")
sizs <- c(rep(0.3, 4), 0.6, 0.6)
alph <- c(rep(0.4, 4), 0.6, 0.6)

db4 %>% 
  filter(type == "mx_pclm_par",
         Country %in% cts) %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Year, size = Year, alpha = Year))+
  facet_grid(Country ~ Sex)+
  scale_y_log10()+
  scale_color_manual(values = cols)+
  scale_size_manual(values = sizs)+
  scale_alpha_manual(values = alph)+
  theme_bw()

ggsave(paste0("Figures/02_example_age_pattern_year_comparison.png"))

db4 %>% 
  filter(type == "mx_pclm_par",
         Country %in% cts,
         Age <= 25) %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Year, size = Year, alpha = Year))+
  facet_grid(Country ~ Sex)+
  scale_y_log10()+
  scale_color_manual(values = cols)+
  scale_size_manual(values = sizs)+
  scale_alpha_manual(values = alph)+
  theme_bw()

ggsave(paste0("Figures/03_example_age_pattern_year_comparison_young.png"))

# problem of using average
sizs <- c(rep(0.5, 4), 0.6, 0.6)
cols <- c("#52b788", "#40916c", "#2d6a4f", "#1b4332", "red", "blue")

db4 %>% 
  filter(type == "mx_pclm_par",
         Country %in% c("Chile"),
         Age <= 25) %>% 
  ggplot()+
  geom_line(aes(age_mid, mx, col = Year, size = Year, alpha = Year))+
  facet_grid(~Sex)+
  scale_y_log10()+
  scale_size_manual(values = sizs)+
  scale_color_manual(values = cols)+
  scale_alpha_manual(values = alph)+
  theme_bw()

ggsave(paste0("Figures/04_problem_average.png"))


db5 <- 
  db4 %>% 
  filter(Year %in% c("2020", "Av"),
         type == "mx_pclm_par") %>% 
  spread(Year, mx) %>%
  rename(y2020 = '2020') %>% 
  mutate(ratio = y2020 / Av)


tx <- 6


db5 %>% 
  filter(Age < 25, 
         Country == "Chile") %>% 
  ggplot()+
  geom_line(aes(Age, ratio))+
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.3)+
  scale_y_log10()+
  facet_wrap( ~ Sex)+
  theme_minimal()+
  theme(strip.text.x = element_text(margin = margin( b = 0, t = 0)),
        strip.text = element_text(size = tx),
        axis.text.y = element_text(size = tx - 1),
        axis.text.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx),
        axis.title.x = element_text(size = tx),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.1)
  )
ggsave(paste0("Figures/05_ratio_chile.png"))


unique(db5$Country)

db5 %>% 
  filter(Sex == "f",
         Age < 25) %>% 
  ggplot()+
  geom_line(aes(Age, ratio))+
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.3)+
  scale_y_log10()+
  facet_wrap(~ Country)+
  theme_minimal()+
  theme(strip.text.x = element_text(margin = margin( b = 0, t = 0)),
        strip.text = element_text(size = tx),
        axis.text.y = element_text(size = tx - 1),
        axis.text.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx),
        axis.title.x = element_text(size = tx),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.1)
  )

ggsave(paste0("Figures/05_ratio_young_all_f.png"))



db5 %>% 
  filter(Country %in% cts,
         Age <= 25) %>% 
  ggplot()+
  geom_line(aes(Age, ratio))+
  facet_grid(Country ~ Sex)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()+
  
  

ggsave(paste0("Figures/05_ratio_young.png"))


cts2 <- c("Spain", "Germany", "France", "Italy", "Netherlands") 

db5 %>% 
  filter(Country %in% cts2,
         Age <= 25) %>% 
  ggplot()+
  geom_line(aes(Age, ratio))+
  facet_grid(Country ~ Sex)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

ggsave(paste0("Figures/06_ratio_young_others.png"))

unique(db5$Country)
cts2 <- c("Spain", "Germany", "France", "Belgium", "Netherlands") 

db5 %>% 
  filter(Country %in% cts2,
         Age <= 25) %>% 
  ggplot()+
  geom_line(aes(Age, ratio))+
  facet_grid(Country ~ Sex)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

ggsave(paste0("Figures/07_ratio_young_others.png"))

# 
# 
# 
# db2 %>% 
#   filter(Country == c,
#          Sex == s,
#          type == "mx_pclm_exp") %>% 
#   ggplot()+
#   geom_line(aes(age_mid, mx, col = Year), alpha = 0.5)+
#   scale_y_log10()
# 
# db2 %>% 
#   filter(Country == c,
#          Sex == s,
#          type == "mx_pclm_par") %>% 
#   ggplot()+
#   geom_line(aes(age_mid, mx, col = Year), alpha = 0.5)+
#   scale_y_log10()
# 
# db2 %>% 
#   filter(Country == c,
#          Sex == s,
#          type == "observed") %>% 
#   ggplot()+
#   geom_line(aes(age_mid, mx, col = Year), alpha = 0.5)+
#   scale_y_log10()
# 
# 
# db2 %>% 
#   filter(Country == c,
#          Sex == s,
#          Year == y) %>% 
#   ggplot()+
#   geom_line(aes(age_mid, mx, col = type), alpha = 0.5)+
#   scale_y_log10()
# 

