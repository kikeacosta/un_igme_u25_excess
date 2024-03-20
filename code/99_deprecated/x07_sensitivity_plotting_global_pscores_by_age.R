rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit_indep <- read_rds("Output/sens_analysis_global_p_scores_excess_deaths_cts_independent.rds")
all_fit_17_20 <- read_rds("Output/sens_analysis_global_p_scores_excess_deaths_cts_2017_2020.rds")
all_fit_17_21 <- read_rds("Output/sens_analysis_global_p_scores_excess_deaths_cts_2017_2021.rds")

# loading total population by income
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")

income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low")

# function for plotting global P-scores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_pscs <- function(db){
  pscs <- 
    db %>% 
    filter(Income != "No income data") %>% 
    left_join(pop_income) %>% 
    mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                           up < 1 & lp < 1 ~ "Negative",
                           TRUE ~ "No-excess"),
           out = ifelse(exc == "No-excess", 0.5, 1),
           ins = ifelse(exc == "No-excess", 0.6, 0.4),
           Year = Year %>% as.character(),
           Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                        "10-14", "15-19", "20-24")),
           age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
           age_type = factor(age_type, levels = rev(c("Inf", "5y"))),
           Income = factor(Income, levels = income_lvs),
           prop_pop = paste0("(", round(Exposure / tot_pop, 2) * 100, "%)"))
  
  bks <- c(0.5, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.5, 2)
  lbs <- paste0((bks - 1)*100, "%")
  
  tx <- 10
  cols <- c("Positive" = "#b7094c",
            "Negative" = "#0091ad",
            "No-excess" = "#5c4d7d")
  
  plot <- 
    pscs %>% 
    filter(Sex == "t") %>% 
    ggplot()+
    geom_point(aes(psc, Age, 
                   alpha = out, 
                   col = exc))+
    geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                       alpha = out, 
                       col = exc))+
    geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
    geom_text(aes(0.67, Age, label = prop_pop), size = 2.5, hjust = 0)+
    facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
    scale_x_log10(breaks = bks, labels = lbs,
                  limits = c(1/1.5, 1.5))+
    scale_y_discrete(limits=rev) %>% 
    scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
    scale_color_manual(values = cols, guide = "none")+
    labs(x = "Global p-score",
         y = "Age")+
    theme_bw()+
    theme(legend.position = "right",
          legend.text = element_text(size = tx),
          legend.title = element_text(size = tx),
          strip.text.y = element_text(size = tx - 2),
          strip.text.x = element_text(size = tx - 2),
          panel.spacing.y = unit(0,"lines"),
          axis.text.x = element_text(size = tx - 3.5),
          axis.title.x = element_text(size = tx - 1),
          axis.title.y = element_text(size = tx - 1),
          axis.text.y = element_text(size = tx - 2))
  
  return(plot)

  }
1/1.5
plot_pscs(all_fit_indep)
ggsave("Figures/manuscript/figS12_sens_global_pscores_independent.png", 
       dpi = 700, width = 8, height = 5)

plot_pscs(all_fit_17_20)
# ggsave("Figures/manuscript/figS11_global_pscores_cts_17_20.png", 
#        dpi = 600, width = 8, height = 5)

plot_pscs(all_fit_17_21)
ggsave("Figures/manuscript/figS13_sens_global_pscores_cts_17_21.png", 
       dpi = 700, width = 8, height = 5)


