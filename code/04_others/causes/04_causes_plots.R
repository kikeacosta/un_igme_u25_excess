library(here)
source(here("Code", "00_functions.R"))

db_c_ec <- read_rds("Output/causes_changes_2020_ecuador.rds")
db_c_br <- read_rds("Output/causes_changes_2020_brazil.rds")
db_c_cr <- read_rds("Output/causes_changes_2020_costa_rica.rds")
causes_cods <- read_xlsx("Data/becker_codes.xlsx") 

causes_cods2 <- 
  causes_cods %>% 
  mutate(Cause = paste0("c", id)) %>% 
  select(Cause, causes, codes)


db_cs <- 
  bind_rows(db_c_ec %>%
              mutate(Country = "Ecuador"),
            db_c_cr %>%
              mutate(Country = "Costa Rica"),
            db_c_br %>%
              mutate(Country = "Brazil")) %>% 
  left_join(causes_cods2) %>% 
  mutate(Cause_des = case_when(Cause == 'c5' ~ 'Meningitis', 
                               Cause == 'c18' ~ 'Cancer uterus', 
                               Cause == 'c24' ~ 'Cancer lymphoid', 
                               Cause == 'c28' ~ 'Dehydratation',
                               Cause == 'c36' ~ 'Cardiorespiratory', 
                               Cause == 'c46' ~ 'Influenza and Pneumonia', 
                               Cause == 'c48' ~ 'Pulmonary oedema', 
                               Cause == 'c49' ~ 'Respiratory failure', 
                               Cause == 'c53' ~ 'Urinary syst', 
                               Cause == 'c55' ~ 'Perinatal', 
                               Cause == 'c56' ~ 'Congenital', 
                               Cause == 'c57' ~ 'Traffic accidents', 
                               Cause == 'c6' ~ 'Septicaemia', 
                               Cause == 'c60' ~ 'Accidental drowning', 
                               Cause == 'c63' ~ 'Suicide', 
                               Cause == 'c64' ~ 'Homicide', 
                               Cause == 'c88' ~ 'Remainder', 
                               Cause == 'c99' ~ 'Ill-defined'))

ext <- c("c57", "c60", "c63", "c64")
res_inf <- c("c6", "c46", "c48", "c49", "c71")

css <- 
  db_cs %>% 
  select(Cause, causes) %>% 
  arrange(Cause) %>% 
  unique()

write.excel(css)

cols <- 
  c(
    # external
    "Traffic accidents" = "#660708", 
    "Accidental drowning" = "#a4161a", 
    "Suicide" = "#ba181b", 
    "Homicide" = "#e5383b",
    # respiratory and infectious
    "Meningitis" = "#007f5f",
    "Septicaemia" = "#2b9348", 
    "Influenza and Pneumonia" = "#80b918", 
    "Pulmonary oedema" = "#bfd200", 
    "Respiratory failure" = "#eeef20", 
    # "Cardiorespiratory" = "#eeef20",
    # congenital / perinatal
    "Perinatal" = "#7b2cbf",
    "Congenital" = "#c77dff",
    # others
    # cancer
    "Cancer lymphoid" = "#01497c",
    # 'Cancer uterus' = "#2a6f97",
    # 'Urinary syst' = "#468faf",
    # 'Dehydratation' = "#89c2d9",
    # rest
    "Ill-defined" = "#936639",
    "Remainder" = "grey"
  )


levs <- 
  c(
    # external
    "Traffic accidents", 
    "Accidental drowning", 
    "Suicide", 
    "Homicide",
    # respiratory and infectious
    "Meningitis",
    "Septicaemia", 
    "Influenza and Pneumonia", 
    "Pulmonary oedema", 
    "Respiratory failure", 
    "Cardiorespiratory",
    # congenital / perinatal
    "Perinatal",
    "Congenital",
    # others
    # cancer
    "Cancer lymphoid",
    'Cancer uterus',
    'Urinary syst',
    'Dehydratation',
    # rest
    "Ill-defined",
    "Remainder"
  )

db_cs %>% 
  mutate(Cause_des = factor(Cause_des, levels = levs)) %>% 
  ggplot()+
  geom_bar(aes(x = factor(Age), y = cont, fill = Cause_des), 
           stat = "identity", 
           position="fill")+
  coord_cartesian(expand = 0)+
  scale_fill_manual(values = cols)+
  facet_grid(~ Country)+
  labs(fill = "Cause",
       x = "Age group",
       y = "Neg. contribution")+
  theme(legend.key.size = unit(0.2, "cm"))

ggsave("Figures/causes/composition_decrease_ecu_bra_cri.png", width = 8, height = 4.09)
