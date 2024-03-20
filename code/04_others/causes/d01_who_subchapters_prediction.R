rm (list = ls())
source("Code/causes/00_functions_cause_analyses.R")

dts <- 
  read_rds("data_inter/deaths_by_cause_who_db_and_exposures.rds")

unique(dts$age)

# removing covid deaths from fitting
covid_dts <- 
  dts %>% 
  filter(cod == "U071") %>% 
  select(country, age, age_type, year, cod, dts, exposure) %>% 
  mutate(bsn = 0,
         ll = 0, 
         ul = 0)

dts2 <- 
  dts %>% 
  filter(cod != "U071")

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
  mutate(cod = "All_cause",
         dts = dts + 1) %>% 
  group_by(country, age) %>% 
  do(fit_annual(chunk = .)) %>% 
  ungroup() %>% 
  mutate(dts = dts - 1,
         bsn = ifelse(bsn < 1, 0, bsn - 1),
         ll = ifelse(ll < 1, 0, ll - 1),
         ul = ifelse(ul < 1, 0, ul - 1)) %>% 
  mutate(pscore = dts / bsn)

# # visual inspection
# bsn_all %>% 
#   filter(country == "Costa Rica",
#          age == "0") %>% 
#   ggplot()+
#   geom_point(aes(year, dts))+
#   geom_line(aes(year, bsn))+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_ribbon(aes(year, ymin = ll, ymax = ul), alpha = 0.3)+
#   theme_bw()


# # toy loop fitting for identifyiing problematic combinations 
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

# contribution by infant ages to total infant mortality change ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
conts_inf_ages <- 
  bsn_all %>% 
  filter(age_type == "infant",
         year == 2020) %>% 
  group_by(country) %>% 
  mutate(dts_tot = sum(dts),
         bsn_tot = sum(bsn),
         psc_tot = dts_tot / bsn_tot) %>% 
  ungroup() %>% 
  mutate(psc_test = (dts_tot / bsn_tot) - 1,
         cont = (dts / bsn - 1) * (dts / dts_tot),
         country2 = paste0(country, " (", round(psc_tot, 2), ")"))
  

conts_inf_ages %>% 
  filter(dts_tot > 50) %>%  
  ggplot(aes(x = cont, y = reorder(country2, psc_tot)))+
  geom_bar(aes(fill = age),
           stat = "identity", 
           # position = "fill",
           position = "stack",
           alpha = 0.8)+
  scale_fill_manual(values = c("#0a9396", "#ae2012"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(title = "Age-specific contribution to excess infant mortality",
       subtitle = "(only countries with >50 infant deaths in 2020)")+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave(paste0("Figures/causes/who_subchp_causes_ages_conts.png"), 
       dpi = 600, width = 10, height = 5)



# predicting 2020 mortality by cause ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_causes <- 
  dts2 %>% 
  mutate(dts = dts + 1) %>% 
  group_by(country, age, cod) %>% 
  # filter(!any(dts == 0)) %>%
  do(fit_annual(chunk = .)) %>% 
  ungroup() %>% 
  mutate(dts = dts - 1,
         bsn = ifelse(bsn < 1, 0, bsn - 1),
         ll = ifelse(ll < 1, 0, ll - 1),
         ul = ifelse(ul < 1, 0, ul - 1))

bsn_causes %>% 
  filter(country == "Mexico",
         age == "0",
         cod == "P20-P29") %>% 
  ggplot()+
  geom_point(aes(year, dts))+
  geom_line(aes(year, bsn))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(aes(year, ymin = ll, ymax = ul), alpha = 0.3)+
  theme_bw()

# bsn_causes <- tibble()
# 
# cts <- unique(dts2$country)
# for(ct in cts){
#   
#   temp1 <-
#     dts2 %>%
#     filter(country == ct)
#   
#   css <- unique(temp1$cod)
#     
#   for(cs in css){
#     temp2 <-
#       temp1 %>%
#       filter(cod == cs)
#     
#     ags <- unique(temp2$age)
#     
#     for(a in ags){
#       temp3 <-
#         temp2 %>%
#         filter(age == a)
#       
#       test <- 
#         fit_annual(temp3) %>% 
#         mutate(country = ct,
#                cod = cs,
#                age = a)
#       
#       bsn_causes <- 
#         bsn_causes %>% 
#         bind_rows(test)
#     }
#   }
# }


unique(bsn_causes$age)
unique(bsn_causes$country)
# adding covid deaths 
bsn_causes2 <- 
  bsn_causes %>% 
  select(-t, -w, -age_exp) %>% 
  bind_rows(covid_dts) 
# %>% 
#   filter(!country %in% c("England and Wales", "Bosnia and Herzegovina", 
#                          "Oman", "Iceland",
#                          "Qatar", "Estonia", "United Arab Emirates"))

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
  ungroup() %>% 
  left_join(bsn_cause_sum) %>% 
  mutate(diff = bsn - bsn_css, 
         ratio_all_sum = bsn / bsn_css,
         pscore_all = dts / bsn,
         pscore_css = dts / bsn_css)

# %>% 
#   filter(age_type == "young")

# pred_inf_comp <- 
#   bsn_all %>% 
#   filter(year == 2020) %>% 
#   bind_rows(covid_dts) %>% 
#   group_by(country, age_type, age) %>% 
#   summarise(dts = sum(dts),
#             bsn = sum(bsn)) %>%
#   ungroup() %>% 
#   left_join(bsn_cause_sum) %>% 
#   mutate(diff = bsn - bsn_css, 
#          ratio_all_sum = bsn / bsn_css,
#          pscore_all = dts / bsn,
#          pscore_css = dts / bsn_css) %>% 
#   filter(age_type == "infant")

cts_to_look <- 
  pred_comp %>% 
  filter(dts > 50,
         !is.na(bsn_css)) %>%
  select(country, age, fact_adj = ratio_all_sum)

chapters <- 
  read_xlsx("Data/causes_of_death/icd10_subchapters.xlsx") %>% 
  select(code_chp, chapter, cod = code_sub_chp, sub_chp, chp_name)

# chapters_names <- 
#   read_xlsx("Data/causes_of_death/icd10_subchapters.xlsx") %>% 
#   select(cod = code_sub_chp, chp_name)

lvs_chps <- 
  chapters %>% 
  arrange(cod) %>% 
  pull(chp_name) %>% 
  unique()

unique(conts$cod)


# cont = (dts / bsn - 1) * (dts / dts_tot)

conts <- 
  bsn_causes2 %>% 
  filter(year == 2020) %>% 
  left_join(cts_to_look) %>% 
  filter(!is.na(fact_adj)) %>% 
  select(-exposure, -ll, -ul) %>% 
  mutate(bsn = bsn * fact_adj) %>% 
  group_by(country, age) %>% 
  mutate(dts_tot = sum(dts),
         bsn_tot = sum(bsn),
         psc_tot = dts_tot / bsn_tot) %>% 
  ungroup() %>% 
  left_join(chapters) %>% 
  mutate(psc_test = (dts_tot / bsn_tot) - 1,
         cont = (dts / bsn - 1) * (dts / dts_tot),
         country2 = paste0(country, " (", round(psc_tot, 2), ")"),
         age = factor(age, levels = c("neo", "post_neo",
                                      "0", "1", "5", "10", "15", '20')),
         chp_name = factor(chp_name, levels = lvs_chps)) %>%
  filter(!is.na(cont),
         cont != Inf)

# conts <- 
#   bsn_causes2 %>% 
#   filter(year == 2020) %>% 
#   left_join(cts_to_look) %>% 
#   filter(!is.na(fact_adj)) %>% 
#   select(-exposure, -ll, -ul) %>% 
#   mutate(bsn = bsn * fact_adj) %>% 
#   mutate(exc = dts - bsn) %>%
#   group_by(country, age) %>% 
#   mutate(bsn_tot = sum(bsn),
#          pscore_tot = sum(dts) / sum(bsn),
#          exc_tot = sum(exc),
#          exc_neg = sum(ifelse(exc < 0, exc, 0)),
#          exc_pos = sum(ifelse(exc > 0, exc, 0))) %>% 
#   ungroup() %>% 
#   mutate(cont = exc / ifelse(exc < 0, -exc_neg, exc_pos),
#          cont = ifelse(is.na(cont), 0, cont),
#          cont2 = (exc + bsn_tot) / bsn_tot,
#          cont2 = ifelse(is.na(cont2), 1, cont2),
#          cont2 = cont2 - 1,
#          cont3 = cont * pscore_tot) %>% 
#   left_join(chapters) %>% 
#   mutate(country2 = paste0(country, " (", round(pscore_tot, 2), ")"),
#          age = factor(age, levels = c("neo", "post_neo",
#                                       "0", "1", "5", "10", "15", '20')),
#          chp_name = factor(chp_name, levels = lvs_chps))

conts_inf <- 
  conts %>%
  filter(age_type == "infant")

unique(conts_inf$age)
min(conts_inf$cont)
max(conts_inf$cont)
summary(conts_inf$cont)


conts_inf %>%
  ggplot(aes(x = cont)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


qt <- 30
# one for decrease of mortality (from green to blue), 
# another for mortality increases (from yellow to red)
col_scale <- 
  c(colorRampPalette(c("#0d47a1", "#e3f2fd"), space = "Lab")(qt),
    colorRampPalette(c("#ffe5ec", "#d90429"), space = "Lab")(qt))

# Definition of brackets for the scale of change
val <- unique(conts_inf$cont)
# separate negative and positive values
pval <- val[val>0] # all positive values of change (mortality deterioration)
nval <- val[val<0] # all negative values of change (mortality improvement)
# identification of brakets for positive values (minimum + 23 quantiles + maximum)
pcop <-c(min(pval), quantile(pval, prob=1/(qt-1)*(1:(qt-2))), max(pval)*1.01)
# the same as above but for negative values (minimum + 23 quantiles + maximum)
ncop <-c(min(nval)*1.01, quantile(nval, prob=1/(qt-1)*(1:(qt-2))), max(nval)*1.01) 
# chain of brakets 25 ranges for negative changes, central value of no change (0), 
# and 25 ranges for positive changes
breaks_mc <- c(ncop, 0, pcop) 

as.numeric(breaks_mc)

# adding to each value of change (continuous) the corresponding 
# bracket (a discrete interval)
conts_inf2 <- 
  conts_inf %>% 
  mutate(cont_cut = cut(conts_inf$cont, 
                      breaks = breaks_mc)) 

cuts <- conts_inf2 %>% pull(cont_cut) %>% unique() %>% sort()
col_brks <- cuts[c(1, 10, 20, 25, 30, 40, 50)]

# assigning a color from our scale to each bracket
col_values <- setNames(col_scale, cuts)

# Plot of mortality change over periods
conts_inf2 %>%
  ggplot()+
  # ggplot(aes(cod, reorder(country2, psc_tot), z = ch_cut))+
  geom_tile(aes(cod, reorder(country2, psc_tot), fill = cont_cut))+
  scale_fill_manual(name = "name_here",
                    breaks = col_brks,
                    values = col_values)+
  facet_nested(age ~ chp_name, scales = "free", space = "free")+
  labs(title = "cause-specific contribution to total excess by age")+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        strip.text.x = element_text(size = 6, angle = 90),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(1,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 4),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/who_subchp_causes_ages_infant.png"), 
       dpi = 600, width = 10, height = 5)
ggsave(paste0("Figures/causes/who_subchp_causes_ages_infant.pdf"), 
       width = 10, height = 5)



breaks_mc <- c(0, seq(0.5, 0.9, 0.1), 
               0.91, 1.1,
               1/rev(seq(0.5, 0.9, 0.1)), 100)








# 

conts_inf <- 
  conts %>%
  filter(age_type == "infant") %>% 
  select(country, country2, age, cod, psc_tot, cont) %>% 
  mutate(cont2 = cont + 1)














conts_inf_sel <- 
  conts %>% 
  filter(age_type == "infant") %>% 
  group_by(age_type, cod) %>% 
  filter(any(cont > 0.05)) %>% 
  ungroup() 
  
conts_inf_sel %>%
  ggplot()+
  geom_tile(aes(sub_chp, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_nested(age ~ chp_name, scales = "free", space = "free")+
  labs(title = "cause-specific contribution to total excess by age")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        strip.text.x = element_text(size = 10, angle = 45),
        strip.text.y = element_text(size = 12, angle = 90),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(1,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/who_subchp_causes_ages_infant_selected.png"), 
       dpi = 600, width = 13, height = 10)
ggsave(paste0("Figures/causes/who_subchp_causes_ages_infant_selected.pdf"), 
       width = 13, height = 10)



# broad perspective
# ~~~~~~~~~~~~~~~~~

conts %>% 
  filter(!age_type == "infant") %>%
  ggplot()+
  geom_tile(aes(cod, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_nested(age ~ chp_name, scales = "free", space = "free")+
  labs(title = "cause-specific contribution to total excess by age")+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        strip.text.x = element_text(size = 10, angle = 90, face="bold"),
        # strip.text.x = element_text(size = 6, angle = 90),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0.4,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/f03_who_subchp_causes_ages_0_24.png"), 
       dpi = 600, width = 14, height = 8)
ggsave(paste0("Figures/causes/f03_who_subchp_causes_ages_0_24.pdf"), 
       width = 14, height = 8)


conts_all_sel <- 
  conts %>% 
  filter(!age_type == "infant") %>% 
  group_by(cod) %>% 
  filter(any(abs(cont) > 0.05)) %>% 
  ungroup() 

conts_all_sel <- 
  conts %>% 
  filter(!age_type == "infant") %>% 
  group_by(cod) %>% 
  filter(mean(abs(cont)) > 0.02) %>% 
  ungroup() 

conts_all_sel %>%
  ggplot()+
  geom_tile(aes(cod, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_nested(age ~ chp_name, scales = "free", space = "free")+
  labs(title = "cause-specific contribution to total excess by age")+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        strip.text.x = element_text(size = 10, angle = 90, face="bold"),
        # strip.text.x = element_text(size = 6, angle = 90),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0.4,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/f04_who_subchp_causes_ages_0_24.png"), 
       dpi = 600, width = 10, height = 8)
ggsave(paste0("Figures/causes/f04_who_subchp_causes_ages_0_24.pdf"), 
       width = 10, height = 8)



conts_all_sel %>%
  ggplot()+
  geom_tile(aes(sub_chp, reorder(country2, pscore_tot), fill = cont))+
  scale_fill_gradient2(
    low = muted("blue"),
    mid = "white",
    high = muted("red"),
    midpoint = 0)+
  facet_nested(age ~ chp_name, scales = "free", space = "free")+
  labs(title = "cause-specific contribution to total excess by age")+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 10),
        strip.text.x = element_text(size = 10, angle = 90, face="bold"),
        # strip.text.x = element_text(size = 6, angle = 90),
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0.4,"lines"),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_blank())

ggsave(paste0("Figures/causes/f05_who_subchp_causes_ages_0_24_long.png"), 
       dpi = 600, width = 10, height = 8)
ggsave(paste0("Figures/causes/f05_who_subchp_causes_ages_0_24_long.pdf"), 
       width = 10, height = 8)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pscores in ill-defined causes of death
abnormals <- 
  conts %>% 
  filter(chp_name == "abnormal") %>% 
  group_by(country, age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  mutate(pscore = dts / bsn) %>% 
  filter(dts >= 30)

cols <- 
  c("#d9ed92",
    "#b5e48c",
    "#99d98c",
    "#76c893",
    "#52b69a",
    "#34a0a4",
    "#168aad",
    "#1e6091")

abnormals %>% 
  ggplot()+
  geom_point(aes(pscore, country, col = age))+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_colour_manual(values = cols)+
  theme_bw()+
  labs(title = "pscores in ill-defined causes of death")
ggsave("Figures/causes/who_subchp_causes_abnormals.png", 
       dpi = 600, width = 5, height = 3)







# 
# 
# 
# 
# # cause changes in ages <1 and 1-4
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# conts %>%
#   filter(age %in% c("0", "1")) %>% 
#   ggplot()+
#   geom_tile(aes(cod, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 6, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(0.5,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 4),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_0_4.png"), 
#        dpi = 600, width = 10, height = 5)
# 
# 
# conts_chl_sel <- 
#   conts %>% 
#   filter(age %in% c("0", "1")) %>% 
#   group_by(age, cod) %>% 
#   filter(any(cont > 0.05)) %>% 
#   ungroup() 
# 
# conts_chl_sel %>%
#   ggplot()+
#   geom_tile(aes(sub_chp, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free_x")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 8, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(1,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_0_4_selected.png"), 
#        dpi = 600, width = 10, height = 10)
# 
# 
# # 
# # cause changes in ages 5-9 and 10-14
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# conts %>%
#   filter(age %in% c("5", "10")) %>% 
#   ggplot()+
#   geom_tile(aes(cod, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free_x")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 6, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(1,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 4),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_5_14.png"), 
#        dpi = 600, width = 10, height = 5)
# 
# 
# conts_5_14_sel <- 
#   conts %>% 
#   filter(age %in% c("5", "10")) %>% 
#   group_by(age, cod) %>% 
#   filter(any(cont > 0.05)) %>% 
#   ungroup() 
# 
# conts_5_14_sel %>%
#   ggplot()+
#   geom_tile(aes(sub_chp, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free_x")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 8, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(1,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_5_14_selected.png"), 
#        dpi = 600, width = 10, height = 10)
# 
# 
# # 
# # cause changes in ages 15-19 and 20-24
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# conts %>%
#   filter(age %in% c("15", "20")) %>% 
#   ggplot()+
#   geom_tile(aes(cod, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free_x")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 6, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(1,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 4),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_15_24.png"), 
#        dpi = 600, width = 10, height = 5)
# 
# 
# conts_5_14_sel <- 
#   conts %>% 
#   filter(age %in% c("15", "20")) %>% 
#   group_by(age, cod) %>% 
#   filter(any(cont > 0.05)) %>% 
#   ungroup() 
# 
# conts_5_14_sel %>%
#   ggplot()+
#   geom_tile(aes(sub_chp, reorder(country2, pscore_tot), fill = cont))+
#   scale_fill_gradient2(
#     low = muted("blue"),
#     mid = "white",
#     high = muted("red"),
#     midpoint = 0)+
#   facet_nested(age ~ chp_name, scales = "free", space = "free_x")+
#   labs(title = "cause-specific contribution to total excess by age")+
#   theme_bw()+
#   theme(legend.position = "none",
#         plot.title = element_text(size = 10),
#         strip.text.x = element_text(size = 8, angle = 90),
#         panel.spacing.x = unit(0,"lines"),
#         panel.spacing.y = unit(1,"lines"),
#         axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
#         axis.text.y = element_text(size = 8),
#         axis.title.y = element_blank())
# 
# ggsave(paste0("Figures/causes/who_subchp_causes_ages_15_24_selected.png"), 
#        dpi = 600, width = 10, height = 10)
