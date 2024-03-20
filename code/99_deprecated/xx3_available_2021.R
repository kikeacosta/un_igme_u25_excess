library(here)
library(tidyverse)
library(countrycode)

stff <- read_csv("Output/data2021/stff_available_2021.csv")
stmf <- read_csv("Output/data2021/stmf_available_2021.csv")

stff2 <- 
  stff %>% 
  mutate(prop = Months / 12) %>% 
  select(Country, prop) %>% 
  mutate(measure = "births")

stmf2 <- 
  stmf %>% 
  mutate(prop = Week / 52) %>% 
  select(Country, prop) %>% 
  mutate(measure = "deaths")

all <- 
  bind_rows(stff2,
            stmf2)

all %>% 
  ggplot()+
  geom_point(aes(prop, Country, col = measure))+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(title = "Available births and deaths in 2021")+
  theme(axis.text = element_text(size = 7))

ggsave(paste0("Figures/last version/y_2021/births_deaths_data_availability_2021.png"), 
       dpi = 600,
       width = 6, height = 4)
