rm (list = ls())
source("code/i00_functions.R")

dt <- read_rds("data_inter/baselines_by_cause_from_two_methods.rds")

bsn_ts <- 
  dt %>% 
  filter(cause == "total",
         year == 2020,
         source == "fractions") %>% 
  select(country, sex, age, bsn_tot = bsn, lp_tot = lp, up_tot = up)

dt2 <- 
  dt %>% 
  left_join(bsn_ts) %>% 
  mutate(psc = dts/bsn,
         psc_lp = dts/up,
         psc_up = dts/lp,
         psc_cont = exc/bsn_tot+1,
         psc_cont_lp = exc/up_tot+1,
         psc_cont_up = exc/lp_tot+1) %>% 
  mutate(exc_type = case_when(psc < 1 & dts <= lp ~ "out",
                              psc < 1 & dts > lp ~ "within",
                              psc > 1 & dts < up ~ "within",
                              psc > 1 & dts >= up ~ "out",
                              cause == "covid19" ~ "out"),
         exc_type = factor(exc_type, levels = c("within",
                                                "out")))

psc_tots <- 
  dt2 %>% 
  filter(cause == "total",
         source == "baselines")



# cols <- c("positive" = "#b7094c",
#           "no_exc" = "#5c4d7d",
#           "negative" = "#0091ad")

bks <- c(0.8, 0.9, 1, 1.1, 1.2, 1.5)
lbs <- paste0((bks-1)*100, "%")

dt2 %>% 
  filter(sex == "t",
         cause != "total",
         source == "fractions") %>% 
  # mutate(psc_cont = psc_cont+1) %>% 
  ggplot()+
  geom_bar(aes(fill = cause, y = psc_cont, x = cause), stat = "identity")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_point(data = psc_tots %>% filter(sex == "t"),
             aes(y = psc+1, x = "total", col = exc_type))+
  scale_y_log10(limits = c(0.7, 1.5),
                breaks = bks, labels = lbs)+
  coord_flip()+
  scale_color_manual(values = cols)+
  facet_nested(age~country, scales = "free_y")+
  theme_bw()+
  labs(title = "cause-specific p-score contribution")+
  theme(legend.position = "none",
        axis.text = element_text(size = 7))


# comparing both methods ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cause-specific p-scores
# ~~~~~~~~~~~~~~~~~~~~~~~
bks <- c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)
lbs <- paste0((bks-1)*100, "%")

cols_col <- c("#ff595e", "#1982c4")
dt2 %>% 
  filter(sex == "t",
         cause != "total",
         cause != "covid19",
         country == "Colombia"
  ) %>% 
  ggplot()+
  geom_point(aes(cause, psc, col = source, alpha = exc_type))+
  geom_errorbar(aes(cause, ymin = psc_lp, ymax = psc_up, col = source),
                alpha = 0.7, width=.2)+
  geom_point(data = psc_tots %>% filter(sex == "t", country == "Colombia"),
             aes(y = psc, x = "total"), col = "black")+
  geom_errorbar(data = psc_tots %>% filter(sex == "t", country == "Colombia"),
                aes(cause, ymin = psc_lp, ymax = psc_up),
                col = "black", alpha = 0.7, width=.2)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(
    # limits = c(0.5, 1.5),
                breaks = bks, labels = lbs)+
  scale_color_manual(values = cols_col)+
  scale_alpha_manual(values = c(0.3, 0.9))+
  coord_flip()+
  facet_nested(fct_rev(as.factor(age))~., scales = "free_y", space = "free_y")+
  theme_bw()+
  labs(title = "cause-specific p-score",
       y = "p-score",
       color = "Method",
       alpha = "excess\ntype")+
  theme(axis.text = element_text(size = 7),
        axis.title.y = element_blank())

ggsave("figures/causes_fractions/cause_psc_colombia.png",
       w = 6, h = 4)


# cause-specific contributions to p-scores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bks <- c(0.8, 0.9, 1, 1.1, 1.2, 1.5)
lbs <- paste0((bks-1)*100, "%")

dt2 %>% 
  filter(sex == "t",
         cause != "total",
         country == "Colombia",
         source == "fractions"
  ) %>% 
  ggplot()+
  geom_point(aes(cause, psc_cont, alpha = exc_type), col = "#1982c4")+
  # geom_errorbar(aes(cause, ymin = psc_cont_lp, ymax = psc_cont_up), 
  #               col = "#1982c4", alpha = 0.9, width=.2)+
  geom_point(data = psc_tots %>% filter(sex == "t", country == "Colombia"),
             aes(y = psc, x = "total"), col = "black")+
  geom_errorbar(data = psc_tots %>% filter(sex == "t", country == "Colombia"),
                aes(cause, ymin = psc_cont_lp, ymax = psc_cont_up),
                col = "black", alpha = 0.7, width=.2)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10(limits = c(0.78, 1.23),
                breaks = bks, labels = lbs)+
  scale_color_manual(values = cols_col)+
  scale_alpha_manual(values = c(0.3, 0.9))+
  coord_flip()+
  facet_nested(fct_rev(as.factor(age))~., scales = "free_y", space = "free_y")+
  theme_bw()+
  labs(title = "cause-specific contributions to all-cause p-score",
       y = "contribution to p-score",
       # color = "Method",
       alpha = "excess\ntype")+
  theme(axis.text = element_text(size = 7),
        axis.title.y = element_blank())

ggsave("figures/causes_fractions/cause_cont_colombia.png",
       w = 6, h = 4)

# , alpha = 0.9
# filtering some countries, ages and causes ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Latam countries
gr1 <- c( "Argentina",
          "Brazil", 
          "Colombia",
          "Mexico",
          "Peru")

# high-income countries
gr2 <- c("Netherlands",
         "Spain",
         "Switzerland",
         "United Kingdom",
         "USA")

# high-income countries without excess in 2020
gr3 <- c( "Australia",
          "Austria",
          "Denmark", 
          "Japan")

psc_cts <- 
  dt2 %>% 
  filter(country %in% c(gr1, gr2)) %>% 
  mutate(group = case_when(country %in% gr1 ~ "LATAM", 
                           country %in% gr2 ~ "HIC", 
                           country %in% gr3 ~ "HIC-NExc"))

psc_tots2 <- 
  psc_tots %>% 
  filter(country %in% c(gr1, gr2)) %>% 
  mutate(group = case_when(country %in% gr1 ~ "LATAM", 
                           country %in% gr2 ~ "HIC", 
                           country %in% gr3 ~ "HIC-NExc"))

# contributions by cause
bks <- c(0.7, 1, 1.5)
lbs <- paste0((bks-1)*100, "%")
psc_cts %>%
  filter(source == "fractions",
         cause != "total") %>%
  ggplot()+
  geom_bar(aes(fill = cause, y = psc_cont, x = cause), stat = "identity")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_point(data = psc_tots2 %>% filter(sex == "t"),
             aes(y = psc, x = "total", alpha = exc_type))+
  # geom_errorbar(data = psc_tots2 %>% filter(sex == "t"),
  #               aes(cause, ymin = psc_lp, ymax = psc_up),
  #               col = "black", alpha = 0.7, width=.2)+
  scale_y_log10(breaks = bks, labels = lbs)+
  coord_flip()+
  scale_color_manual(values = cols)+
  scale_alpha_manual(values = c(0.3, 0.9))+
  facet_nested(age~group+country, scales = "free_y")+
  theme_bw()+
  labs(y = "cause-specific contribution to AC p-score")+
  theme(legend.position = "none",
        axis.text = element_text(size = 7))

ggsave("figures/causes_fractions/cause_cont_psc_by_income.png",
       w = 10, h = 6)


# all-cause p-scores for all countries 
{
  
  bks <- c(0.8, 1, 1.2, 1.4)
  lbs <- paste0((bks-1)*100, "%")
  
  psc_cts %>% 
    filter(cause == "total",
           source == "fractions",
           sex == "t") %>% 
    mutate(cause = factor(cause, levels = cs)) %>% 
    ggplot()+
    geom_jitter(aes(psc, age, col = group, alpha = exc_type))+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_x_log10(breaks = bks, labels = lbs)+
    scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
    scale_color_manual(values = c("#bb3e03", "#0a9396", "black"))+
    scale_alpha_manual(values = c(0.3, 0.9))+
    # facet_grid(~cause)+
    theme_bw()+
    labs(x = "P-score",
         color = "country\ngroup",
         alpha = "excess\ntype")
  
  
  ggsave("figures/causes_fractions/all-cause_psc_by_income.png",
         w = 4, h = 3)
  }

# contributions by perinatal, congenital, and respiratory
{
  cs <- c("total", "perinatal", "congenital", "respiratory")
  bks <- c(0.8, 1, 1.2, 1.4)
  lbs <- paste0((bks-1)*100, "%")
  
  psc_cts %>% 
    filter(cause %in% cs,
           source == "fractions",
           sex == "t") %>% 
    mutate(cause = factor(cause, levels = cs)) %>% 
    ggplot()+
    geom_jitter(aes(psc_cont, age, col = group, alpha = exc_type))+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_x_log10(breaks = bks, labels = lbs)+
    scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
    scale_color_manual(values = c("#bb3e03", "#0a9396", "black"))+
    scale_alpha_manual(values = c(0.3, 0.9))+
    facet_grid(~cause)+
    theme_bw()+
    labs(x = "contribution to p-scores",
         color = "country\ngroup",
         alpha = "excess\ntype")
  
  
  ggsave("figures/causes_fractions/perinatal_congenital_psc_by_income.png")
}

# contributions by external causes and C19
{
  cs <- c("total", "external", "covid19")
  bks <- c(0.8, 1, 1.2, 1.4)
  lbs <- paste0((bks-1)*100, "%")
  psc_cts %>% 
    filter(cause %in% cs,
           source == "fractions",
           sex == "t") %>% 
    mutate(psc = psc+1,
           cause = factor(cause, levels = cs)) %>% 
    ggplot()+
    geom_jitter(aes(psc_cont, age, col = group, alpha = exc_type))+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_x_log10(breaks = bks, labels = lbs)+
    scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
    scale_color_manual(values = c("#bb3e03", "#0a9396", "black"))+  
    scale_alpha_manual(values = c(0.3, 0.9))+
    facet_grid(~cause)+
    theme_bw()+
    labs(x = "contribution to p-scores",
         color = "country\ngroup",
         alpha = "excess\ntype")
  
  
    
  ggsave("figures/causes_fractions/external_c19_psc_by_income.png")
}



# contributions by external causes and C19 by sex
{
  cs <- c("total", "external", "covid19")
  bks <- c(0.8, 1, 1.2, 1.5)
  lbs <- paste0((bks-1)*100, "%")
  psc_cts %>% 
    filter(cause %in% cs,
           source == "fractions",
           sex != "t") %>% 
    mutate(psc = psc+1,
           cause = factor(cause, levels = cs)) %>% 
    ggplot()+
    geom_jitter(aes(psc_cont, age, col = group, alpha = exc_type))+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_x_log10(breaks = bks, labels = lbs)+
    scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
    scale_color_manual(values = c("#bb3e03", "#0a9396"))+  
    scale_alpha_manual(values = c(0.3, 0.9))+
    facet_grid(sex~cause)+
    theme_bw()+
    labs(x = "contribution to p-scores",
         color = "country\ngroup",
         alpha = "excess\ntype")
  
  ggsave("figures/causes_fractions/external_c19_psc_by_sex_income.png", 
         w = 7, h = 4)
}


# test <- 
#   psc_cts %>% 
#   filter(cause %in% cs,
#          source == "fractions",
#          sex == "t")
# 
# # contributions by external causes and C19 by sex
# {
#   cs <- c("total")
#   bks <- c(0.8, 1, 1.2, 1.5)
#   lbs <- paste0((bks-1)*100, "%")
#   psc_cts %>% 
#     filter(cause %in% cs,
#            source == "fractions") %>% 
#     mutate(psc = psc+1,
#            cause = factor(cause, levels = cs)) %>% 
#     ggplot()+
#     geom_jitter(aes(psc_cont, age, col = group), alpha = 0.7)+
#     geom_vline(xintercept = 1, linetype = "dashed")+
#     scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
#     scale_x_log10(breaks = bks, labels = lbs)+
#     scale_color_manual(values = c("#bb3e03", "#0a9396"))+  
#     facet_grid(cause~sex)+
#     theme_bw()
#   
#   ggsave("figures/causes_fractions/external_c19_psc_by_sex_income.png")
# }
# test <- 
#   psc_cts %>% 
#   filter(cause %in% cs,
#          source == "fractions",
#          sex == "t")
# 
# # contributions by external causes and C19 by sex
# {
#   cs <- c("total")
#   bks <- c(0.8, 1, 1.2, 1.5)
#   lbs <- paste0((bks-1)*100, "%")
#   psc_cts %>% 
#     filter(cause %in% cs,
#            source == "fractions") %>% 
#     mutate(psc = psc+1,
#            cause = factor(cause, levels = cs)) %>% 
#     ggplot()+
#     geom_jitter(aes(psc_cont, age, col = group), alpha = 0.7)+
#     geom_vline(xintercept = 1, linetype = "dashed")+
#     scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 20))+
#     scale_x_log10(breaks = bks, labels = lbs)+
#     scale_color_manual(values = c("#bb3e03", "#0a9396"))+  
#     facet_grid(cause~sex)+
#     theme_bw()
#   
#   ggsave("figures/causes_fractions/external_c19_psc_by_sex_income.png")
# }
