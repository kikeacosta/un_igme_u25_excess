rm (list = ls())
source("code/i00_functions.R")

dt <- read_rds("data_inter/baselines_2015_2021_sex_age_cause_adj.rds") %>% 
  filter(country != "Germany")

dt2 <- 
  dt %>%
  mutate(inc = ifelse(cause == "total", 0, 1),
         sign = ifelse(exc >= 0, "pos", "neg")) %>% 
  arrange(country, year, sex, age, sign) %>% 
  group_by(country, year, sex, age, sign) %>% 
  mutate(exc_prp = inc * exc / sum(inc * exc)) %>% 
  ungroup() %>% 
  arrange(country, year, sex, age, sign, -exc_prp)

cause_sel <- 
  dt2 %>% 
  filter(!cause %in% c("other", "total")) %>% 
  group_by(country, year, sex, age, sign) %>% 
  mutate(rank = 1:n(),
         exc_prp_cum = cumsum(exc_prp)) %>% 
  ungroup() %>% 
  filter(lag(exc_prp_cum) < 0.8 |
           rank == 1 | 
           (rank == n() & exc_prp_cum < 0.8) | 
           cause %in% c("covid19", "external")) %>% 
  filter(cause %in% c("covid19", "external") | exc_prp >= 0.1) %>% 
  select(country, year, cause, sex, age)

dt3 <- 
  # others
  dt2 %>% 
  filter(cause != "total") %>% 
  anti_join(cause_sel) %>% 
  group_by(country, year, sex, age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            lp = sum(lp),
            up = sum(up),
            exc = sum(exc)) %>% 
  ungroup() %>% 
  mutate(cause = "other") %>% 
  bind_rows(dt2 %>% 
              inner_join(cause_sel) %>% 
              select(country, year, cause, sex, age, dts, bsn, lp, up, exc)) %>% 
  bind_rows(dt2 %>%
              filter(cause == "total") %>% 
              select(country, year, cause, sex, age, dts, bsn, lp, up, exc)) %>% 
  arrange(country, year, sex, age) 


# population data
# ~~~~~~~~~~~~~~~
pop <- read_rds("data_inter/exposures_5-year_groups.rds") %>% 
  rename_with(tolower) %>% 
  select(-source) %>% 
  rename(pop = population)

dt4 <- 
  dt3 %>% 
  # filter(age <= 20) %>% 
  left_join(pop) %>% 
  arrange(country, year, sex, age, cause) %>% 
  mutate(exc_r = 1e5*exc/pop)

# cause-specific decomposition of total p-scores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(dt4$year)
pscs <- 
  dt4 %>% 
  filter(cause != "total") %>% 
  group_by(country, year, sex, age) %>% 
  mutate(dts_tot = sum(dts),
         bsn_tot = sum(bsn),
         exc_tot = sum(exc),
         exc_abs = abs(exc),
         exc_abs_tot = sum(exc_abs),
         psc = dts / bsn,
         psc_tot = dts_tot / bsn_tot,
         psc_cnt = (exc / exc_tot) * psc_tot,
         psc_cnt2 = (exc / exc_abs_tot) * psc_tot,
         psc_cnt2_tot = sum(psc_cnt2),
         psc_cnt3 = psc_cnt2 * psc_tot/psc_cnt2_tot,
         psc_test = sum(psc_cnt),
         psc_test2 = sum(psc_cnt2),
         psc_test3 = sum(psc_cnt3)) %>% 
  ungroup()

psc_tots <- 
  pscs %>% 
  select(country, sex, age, psc_tot) %>% 
  unique() %>% 
  mutate(cause = "total")

unique(pscs$cause)

pscs %>% 
  filter(sex == "t", cause != "total") %>%
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=psc),
           position="stack", stat="identity")+
  geom_point(data = psc_tots %>%
               filter(sex == "t"),
             aes(psc_tot, cause))+
  facet_nested(age~country, scale = "free", space = "free")+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")

pscs %>% 
  filter(sex == "t", cause != "total",
         country != "Brazil") %>%
  mutate(psc = dts / bsn) %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=psc),
           position="dodge", stat="identity")+
  # geom_point(data = psc_tots %>%
  #              filter(sex == "t"),
  #            aes(psc, cause))+
  facet_nested(age~country, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")


# cause-specific contributions
dt4 %>% 
  filter(sex == "t", cause != "total", age == 0) %>%
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc_r),
           position="dodge", stat="identity")+
  geom_point(data = dt4 %>%
               filter(sex == "t", cause == "total", age == 0),
             aes(exc_r, cause))+
  facet_nested(~country, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")

dt4 %>% 
  filter(sex == "t", 
         cause != "total",
         age > 0) %>%
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc_r),
           position="dodge", stat="identity")+
  geom_point(data = dt4 %>%
               filter(sex == "t", 
                      cause == "total",
                      age > 0),
             aes(exc_r, cause))+
  facet_nested(age~country, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")

dt4 %>% 
  filter(sex == "t", cause != "total") %>%
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc_r),
           position="dodge", stat="identity")+
  geom_point(data = dt4 %>%
               filter(sex == "t", cause == "total"),
             aes(exc_r, cause))+
  facet_nested(country~age, scale = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")

# 
unique(dt4$country)
highs <- c("Australia", "Austria", "Denmark", "Germany", 
           "Japan", "Netherlands", "Spain", "Switzerland", 
           "United Kingdom", "USA")

dt5 <- 
  dt4 %>% 
  filter(cause != "total") %>% 
  mutate(exc_abs = abs(exc)) %>% 
  group_by(country, year, sex, age) %>% 
  mutate(exc_prp = exc_abs / sum(exc_abs)) %>% 
  ungroup() %>% 
  arrange(country, year, sex, age,-exc_prp) %>% 
  mutate(income = ifelse(country %in% highs, "high", "middle"))

# av_prop <- 
#   dt5 %>% 
#   group_by(income, sex, age, cause) %>% 
#   summarise(exc_prp = mean(exc_prp)) %>% 
#   ungroup() %>% 
#   arrange(income, sex, age,-exc_prp)
# 
# prop_abs <- 
#   dt5 %>% 
#   group_by(income, sex, age, cause) %>% 
#   summarise(exc_abs = sum(exc_abs)) %>% 
#   ungroup() %>% 
#   group_by(income, sex, age) %>% 
#   mutate(cont = exc_abs / sum(exc_abs)) %>% 
#   ungroup() %>% 
#   arrange(income, sex, age,-cont)
# 
# prop_abs %>% 
#   filter(sex == "t",
#          cont > 0.1 | cause %in% c("covid19", "external")) %>% 
#   ggplot()+
#   geom_bar(aes(fill=cause, y=cause, x=cont),
#            position="dodge", stat="identity")+
#   facet_nested(age~income, scale = "free", space = "free")+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   theme_bw()+
#   theme(legend.position = "none")
# 
# cause_av <- 
#   prop_abs %>% 
#   filter(sex == "t") %>% 
#   group_by(income, cause) %>% 
#   summarise(av_cont = mean(cont))
# 
# cause_av_pos <- 
#   prop_pos %>% 
#   filter(sex == "t") %>% 
#   group_by(income, cause) %>% 
#   summarise(av_cont = mean(cont))
# 
# cause_av_pos %>% 
#   ggplot()+
#   geom_point(aes(av_cont, cause, col = income))+
#   theme_bw()
# 
# prop_pos %>% 
#   filter(sex == "t") %>% 
#   ggplot()+
#   geom_point(aes(cont, cause, col = income))+
#   theme_bw()+
#   facet_grid(~age)
# 
# unique(dt5$cause)
# conts <- 
#   dt5 %>% 
#   filter(sex == "t",
#          cont < 0.1) %>% 
#   mutate(sign = ifelse(exc > 0, "pos", "neg")) %>% 
#   group_by(country, age, sign) %>% 
#   mutate(cont = exc / sum(exc)) %>% 
#   ungroup() %>% 
#   mutate(cont = ifelse(sign == "pos", cont, -cont))
#   
# conts %>% 
#   ggplot()+
#   geom_jitter(aes(cont, cause, col = income, shape = sign), height = NULL)+
#   geom_violin(aes(cont, cause, col = income, group = interaction(cause, sign, income, age)))+
#   theme_bw()+
#   facet_grid(~age)+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   scale_shape_manual(values = c(1, 16))


prop_pos <- 
  dt5 %>% 
  filter(exc > 0) %>% 
  group_by(income, sex, age, cause) %>% 
  summarise(exc = sum(exc)) %>% 
  ungroup() %>% 
  group_by(income, sex, age) %>% 
  mutate(cont = exc / sum(exc)) %>% 
  ungroup() %>% 
  arrange(income, sex, age,-cont)

prop_pos %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=cont),
           position="dodge", stat="identity")+
  facet_nested(income~age, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(title = "cause-specific contribution to excess")+
  theme_bw()+
  theme(legend.position = "none")

prop_pos %>% 
  filter(
    sex == "t",
    # cause %in% c("covid19", "external")
  ) %>% 
  ggplot()+
  geom_point(aes(cont, age, col = income))+
  facet_grid(~cause)

css_pos <- c("external", "covid19", "cancer", "perinatal")

prop_pos %>% 
  filter(sex == "t",
         cause %in% css_pos) %>% 
  mutate(cont = ifelse(income == "middle", -cont, cont),
         income = factor(income, levels = c("middle", "high"))) %>% 
  ggplot(aes(factor(age), cont, fill = income))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(~cause)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1), 
                     labels = c("100%", "50%", "0%", "50%", "100%"))+
  scale_fill_manual(values = c("#bb3e03", "#0a9396"))+
  labs(
    # title = "cause-age-specific contributions to excess mortality",
    x = "Age")+
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "transparent",
                                        colour = "transparent"))

ggsave("figures/causes2/causes_cont_excess_income.png",
       w = 6, h = 3)

prop_pos %>% 
  filter(sex == "t",
         cause %in% css_pos) %>% 
  mutate(cont = ifelse(income == "middle", -cont, cont)) %>% 
  ggplot(aes(factor(age), cont, fill = income, col = cause))+
  geom_bar(position="stack", stat="identity")+
  # facet_grid(~cause)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # labs(title = "cause-specific contributions to excess mortality")+
  scale_fill_manual(values = c("transparent", "grey"))+
  theme_bw()



# contributions to deficits
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
prop_neg <- 
  dt5 %>% 
  filter(exc < 0) %>% 
  group_by(income, sex, age, cause) %>% 
  summarise(exc = sum(exc)) %>% 
  ungroup() %>% 
  group_by(income, sex, age) %>% 
  mutate(cont = exc / sum(exc)) %>% 
  ungroup() %>% 
  arrange(income, sex, age,-cont)

prop_neg %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=cont),
           position="dodge", stat="identity")+
  facet_nested(age~income, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(title = "cause-specific contribution to deficits")+
  theme_bw()+
  theme(legend.position = "none")


# infant mortality
css_neg <- c("perinatal", "congenital", "external", "cancer")

prop_neg %>% 
  filter(sex == "t",
         # age == 0,
         cause %in% css_neg) %>% 
  mutate(cont = ifelse(income == "middle", -cont, cont),
         income = factor(income, levels = c("middle", "high"))) %>% 
  ggplot(aes(factor(age), cont, fill = income))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(~cause)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1), 
                     labels = c("100%", "50%", "0%", "50%", "100%"))+
  scale_fill_manual(values = c("#bb3e03", "#0a9396"))+
  labs(
    # title = "cause-age-specific contributions to mortality deficits",
    x = "Age")+
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "transparent",
                                        colour = "transparent"))

ggsave("figures/causes2/causes_cont_deficits_income.png",
       w = 6, h = 3)







# pscores by income
pscs <- 
  dt %>% 
  filter(cause == "total") %>% 
  mutate(psc = dts / bsn, 
         income = ifelse(country %in% highs, "high", "middle"),
         income = factor(income, levels = c("middle", "high"))) %>% 
  filter(sex == "t",
         country != "Germany") %>% 
  select(country, age, psc, income)

psc_inc <- 
  bind_rows(
    pscs %>% 
      group_by(income, age) %>% 
      summarise(psc = mean(psc)) %>% 
      ungroup() %>% 
      mutate(country = "Average") %>% 
      select(country, age, psc, income))


bks <- c(0.5, 0.7, 1, 1.5, 2, 2.5)
lbs <- paste0((bks - 1)*100, "%")
pscs%>% 
  ggplot()+
  geom_jitter(aes(psc, factor(age), col = income), shape = 16,
              alpha = 0.2)+
  geom_point(data = psc_inc, 
             aes(psc, factor(age), col = income), shape = 16,
             size = 3)+
  scale_x_log10(limits = c(0.5, 2),
                breaks = bks, labels = lbs)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_color_manual(values = c("#bb3e03", "#0a9396"))+
  theme_bw()+
  labs(y = "Age", x = "Mortality disturbances")

ggsave("figures/pscores_income.png",
       w = 6, h = 6)

# 

unique(dt5$country)

# disturbances in rates
# ~~~~~~~~~~~~~~~~~~~~~~
css <- c("cancer", "respiratory", "external", "circulatory", "covid19")

dists <- 
  dt4 %>% 
  filter(sex == "t",
         cause %in% css) %>% 
  mutate(income = ifelse(country %in% highs, "high", "middle"),
         income = factor(income, levels = c("middle", "high"))) %>% 
  group_by(income, age, cause) %>% 
  summarise(exc_r = mean(exc_r)) %>% 
  ungroup()

dists_all_cause <- 
  dt4 %>% 
  filter(sex == "t",
         cause == "total") %>% 
  mutate(income = ifelse(country %in% highs, "high", "middle"),
         income = factor(income, levels = c("middle", "high"))) %>% 
  group_by(income, age, cause) %>% 
  summarise(exc_r = mean(exc_r)) %>% 
  ungroup()


dists %>% 
  # mutate(cont = ifelse(income == "middle", -cont, cont),
  #        income = factor(income, levels = c("middle", "high"))) %>% 
  ggplot()+
  geom_bar(aes(factor(age), exc_r, fill = cause),
           position="stack", stat="identity")+
  geom_point(data = dists_all_cause, aes(factor(age), exc_r))+
  facet_grid(~income)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Age", y = "Mortality disturbances (/100K)")+
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "transparent",
                                        colour = "transparent"))


dists %>% 
  filter(age %in% 15:50) %>% 
  ggplot()+
  geom_bar(aes(factor(age), exc_r, fill = cause),
           position="stack", stat="identity")+
  geom_point(data = dists_all_cause %>% filter(age %in% 15:50), 
             aes(factor(age), exc_r))+
  facet_grid(~income)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Age", y = "Mortality disturbances (/100K)")+
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "transparent",
                                        colour = "transparent"))





av_prop %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc_prp),
           position="dodge", stat="identity")+
  facet_nested(~age, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "none")



# 
# tots <- 
#   dt2 %>% 
#   filter(cause == "total") %>% 
#   select(country, year, sex, age, exc_tot = exc)
# 
# test <- 
#   dt3 %>% 
#   left_join(tots) %>% 
#   arrange(country, year, sex, age) %>% 
#   group_by(country, year, sex, age) %>% 
#   mutate(exc_sum = sum(exc))
# # filter(exc_prp_cum >= 0.8 | lead(exc_prp_cum) > 0.8)
# 
# dt3 %>% 
#   filter(sex == "t") %>%
#   ggplot()+
#   geom_bar(aes(fill=cause, y=cause, x=exc),
#            position="dodge", stat="identity")+
#   geom_point(data = dt2 %>% 
#                filter(cause == "total", sex == "t"),
#              aes(exc, cause))+
#   facet_nested(age~country, scale = "free", space = "free")+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   theme_bw()+
#   theme(legend.position = "none")
# 
# ggsave("figures/cause_excess_age.pdf",
#        w = 12,
#        h = 8)
# 
# dt3 %>% 
#   mutate(psc = dts / bsn) %>% 
#   filter(cause != "covid19") %>%
#   ggplot()+
#   geom_point(aes(psc, cause))+
#   facet_nested(age~sex+year, scale = "free", space = "free")+
#   scale_x_log10(breaks = c(0.5, 0.7, 1, 1.2))+
#   geom_vline(xintercept = 1, linetype = "dashed")+
#   theme_bw()+
#   theme(legend.position = "none")
# 
# ggsave("figures/cause_pscore_age.pdf",
#        w = 12,
#        h = 8)
# 
