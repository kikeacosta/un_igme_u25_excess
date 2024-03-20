rm (list = ls())
# source("Code/00_functions.R")
library(cowplot)
library(tidyverse)
library(cartography)
# library(rgdal)
library(tmap)
library(sf)

# loading data
# ~~~~~~~~~~~~
dts <- read_rds("data_inter/summary_data_all.rds")
bts <- read_rds("data_inter/annual_births.rds")

unique(dts$Age)

# loading map data
data(World)
# checking coordinate system
st_crs(World)

# renaming a few countries
World <- 
  World %>% 
  mutate(name = as.character(name),
         name = case_when(name == "Swaziland" ~ "Eswatini",
                          name == "United States" ~ "USA",
                          name == "Korea" ~ "South Korea",
                          name == "Dominican Rep." ~ "Dominican Republic",
                          name == "Czech Rep." ~ "Czechia",
                          name == "Central African Rep." ~ "Central African Republic",
                          name == "Eq. Guinea" ~ "Equatorial Guinea",
                          name == "S. Sudan" ~ "South Sudan",
                          name == "Bosnia and Herz." ~ "Bosnia & Herzegovina",
                          TRUE ~ name)) %>% 
  filter(name != "Antarctica")

# add Robinson projection
world_rob <-
  st_transform(World, 
               "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

unique(World$name)


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# data availability by year
# ~~~~~~~~~~~~~~~~~~~~~~~~~

yrs <- 
  dts %>% 
  select(Country, Year, Source_type) %>% 
  filter(Year %in% 2020:2021) %>% 
  unique() %>% 
  mutate(id = 1) %>% 
  spread(Year, id) %>% 
  mutate(Year = case_when(
    Source_type == "crvs" & `2020` == 1 & `2021` == 1  ~ "Civil Register and Vital Statistics Systems 2020-2021",
                          Source_type == "crvs" & `2020` == 1 & is.na(`2021`) ~ "Civil Register and Vital Statistics Systems 2020",
                          Source_type == "hmis" & `2020` == 1 & `2021` == 1 ~ "Health Management Information System 2020-2021",
                          Source_type == "hmis" & `2020` == 1 & is.na(`2021`) ~ "Health Management Information System 2020"
    ),
    Year = factor(Year, levels = c("Civil Register and Vital Statistics Systems 2020", 
                                   "Civil Register and Vital Statistics Systems 2020-2021",
                                   "Civil Register and Vital Statistics Systems 2020-2022",
                                   "Health Management Information System 2020", 
                                   "Health Management Information System 2020-2021"))) %>% 
  rename(country = Country) %>% 
  select(country, Year)

map_yrs <- 
  left_join(world_rob, yrs,
            by = c('name' = 'country'))

{
  cols <- 
    c("#168aad", "#184e77", "#d9ed92", "#76c893")
  tx <- 10
}
map_yrs %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey60", size = 0.1) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("Civil Register and Vital Statistics Systems 2020", 
                               "Civil Register and Vital Statistics Systems 2020-2021", 
                               "Health Management Information System 2020", 
                               "Health Management Information System 2020-2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.text = element_text(size = tx),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        # legend.position = "bottom",
        legend.position = c(0.6, 0),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

ggsave("figures/fig02_map_years.png", dpi = 700,
       w = 8,
       h = 5)


# births 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts_bts <- 
  dts %>% 
  filter(Age %in% c("Infant", "Neonatal", "Stillbirths", "0-4"),
         Year >= 2020) %>% 
  select(Code, Year, Source_type) %>% 
  unique() %>% 
  left_join(bts %>% select(Code, Year) %>% unique() %>% 
              filter(Year >= 2020)) %>% 
  mutate(avail = 1) %>% 
  spread(Year, avail) %>%
  mutate(Year = case_when(Source_type == "crvs" & `2020` == 1 & `2021` == 1 ~ "CRVS 2020 and 2021",
                          Source_type == "crvs" & `2020` == 1 & is.na(`2021`) ~ "CRVS 2020",
                          Source_type == "crvs" & is.na(`2020`) & `2021` == 1 ~ "CRVS 2021",
                          Source_type == "hmis" & `2020` == 1 & `2021` == 1 ~ "HMIS 2020 and 2021",
                          Source_type == "hmis" & `2020` == 1 & is.na(`2021`) ~ "HMIS 2020",
                          Source_type == "hmis" & is.na(`2020`) & `2021` == 1 ~ "HMIS 2021"),
         Year = factor(Year, levels = c("CRVS 2020", "CRVS 2021", "CRVS 2020 and 2021",
                                        "HMIS 2020", "HMIS 2021", "HMIS 2020 and 2021"))) %>% 
  rename(code = Code) %>%
  select(code, Year)

map_bts <- 
  left_join(world_rob, cts_bts, 
            by = c('iso_a3' = 'code'))

cols <- 
  c("#168aad", "#184e77", "#d9ed92", "#76c893")

tx <- 10
bts <- 
  map_bts %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stillbirths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts_sbs <- 
  dts %>% 
  filter(Age %in% c("Stillbirths"),
         Year %in% 2020:2021) %>%
  select(Country, Source_type, Year) %>% 
  unique() %>% 
  mutate(avail = 1) %>% 
  spread(Year, avail) %>%
  mutate(Year = case_when(Source_type == "crvs" & `2020` == 1 & `2021` == 1 ~ "CRVS 2020 and 2021",
                          Source_type == "crvs" & `2020` == 1 & is.na(`2021`) ~ "CRVS 2020",
                          Source_type == "crvs" & is.na(`2020`) & `2021` == 1 ~ "CRVS 2021",
                          Source_type == "hmis" & `2020` == 1 & `2021` == 1 ~ "HMIS 2020 and 2021",
                          Source_type == "hmis" & `2020` == 1 & is.na(`2021`) ~ "HMIS 2020",
                          Source_type == "hmis" & is.na(`2020`) & `2021` == 1 ~ "HMIS 2021"),
         Year = factor(Year, levels = c("CRVS 2020", "CRVS 2021", "CRVS 2020 and 2021",
                                        "HMIS 2020", "HMIS 2021", "HMIS 2020 and 2021"))) %>% 
  rename(country = Country) %>% 
  select(country, Year)

map_sbs <- 
  left_join(world_rob, cts_sbs, 
            by = c('name' = 'country'))

# cols <- 
#   c("#669bbc", "#003049", "#c1121f", "#780000")
# tx <- 10
sbs <- 
  map_sbs %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

for_leg <- 
  map_bts %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.text = element_text(size = tx),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(1, 1, 1, 1),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

legend <- get_legend(for_leg)

plots <- 
  plot_grid(bts, sbs, 
            ncol = 2, 
            labels = c("Births", "Stillbirths"),
            hjust = 0,
            label_size = 10)

plot_grid(plots, legend, 
          ncol = 1, 
          rel_heights = c(1, 0.1))

ggsave("figures/figS01_map_bts_sbs.png",
       dpi = 700,
       w = 8,
       h = 2.7)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure S02: neonatal and infant mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# neonatal
# ~~~~~~~~

cts_neo <- 
  dts %>% 
  filter(Age %in% c("Neonatal"),
         Year %in% 2020:2021) %>%
  select(Country, Source_type, Year) %>% 
  unique() %>% 
  mutate(avail = 1) %>% 
  spread(Year, avail) %>%
  mutate(Year = case_when(Source_type == "crvs" & `2020` == 1 & `2021` == 1 ~ "CRVS 2020 and 2021",
                          Source_type == "crvs" & `2020` == 1 & is.na(`2021`) ~ "CRVS 2020",
                          Source_type == "crvs" & is.na(`2020`) & `2021` == 1 ~ "CRVS 2021",
                          Source_type == "hmis" & `2020` == 1 & `2021` == 1 ~ "HMIS 2020 and 2021",
                          Source_type == "hmis" & `2020` == 1 & is.na(`2021`) ~ "HMIS 2020",
                          Source_type == "hmis" & is.na(`2020`) & `2021` == 1 ~ "HMIS 2021"),
         Year = factor(Year, levels = c("CRVS 2020", "CRVS 2021", "CRVS 2020 and 2021",
                                        "HMIS 2020", "HMIS 2021", "HMIS 2020 and 2021"))) %>% 
  rename(country = Country) %>% 
  select(country, Year) %>% 
  filter(Year != "CRVS 2021")

map_neo <- 
  left_join(world_rob, cts_neo, 
            by = c('name' = 'country'))

# cols <- 
#   c("#669bbc", "#003049", "#c1121f", "#780000")
# 
# tx <- 10
neo <- 
  map_neo %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# infant deaths
# ~~~~~~~~~~~~~

cts_inf <- 
  dts %>% 
  filter(Age %in% c("Infant"),
         Year %in% 2020:2021) %>%
  select(Country, Source_type, Year) %>% 
  unique() %>% 
  mutate(avail = 1) %>% 
  spread(Year, avail) %>%
  mutate(Year = case_when(Source_type == "crvs" & `2020` == 1 & `2021` == 1 ~ "CRVS 2020 and 2021",
                          Source_type == "crvs" & `2020` == 1 & is.na(`2021`) ~ "CRVS 2020",
                          Source_type == "crvs" & is.na(`2020`) & `2021` == 1 ~ "CRVS 2021",
                          Source_type == "hmis" & `2020` == 1 & `2021` == 1 ~ "HMIS 2020 and 2021",
                          Source_type == "hmis" & `2020` == 1 & is.na(`2021`) ~ "HMIS 2020",
                          Source_type == "hmis" & is.na(`2020`) & `2021` == 1 ~ "HMIS 2021"),
         Year = factor(Year, levels = c("CRVS 2020", "CRVS 2021", "CRVS 2020 and 2021",
                                        "HMIS 2020", "HMIS 2021", "HMIS 2020 and 2021"))) %>% 
  rename(country = Country) %>% 
  select(country, Year) %>% 
  filter(Year != "CRVS 2021")

unique(cts_inf$Year)

map_inf <- 
  left_join(world_rob, cts_inf, 
            by = c('name' = 'country'))

inf <- 
  map_inf %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

for_leg <- 
  map_neo %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("CRVS 2020", "CRVS 2020 and 2021",
                               "HMIS 2020", "HMIS 2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  theme(legend.text = element_text(size = tx),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(1, 1, 1, 1),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

legend <- get_legend(for_leg)

plots <- 
  plot_grid(neo, inf, 
            ncol = 2, 
            labels = c("Neonatal mortality", "Infant mortality"),
            hjust = 0,
            label_size = 10)

plot_grid(plots, legend, 
          ncol = 1, 
          rel_heights = c(1, 0.1))

ggsave("figures/figS02_map_neo_inf.png",
       dpi = 700,
       w = 8,
       h = 2.7)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5-year age groups deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(dts$Age)

a5g <- c("0-4", "5-9", "10-14", "15-19", "20-24")

lvs <- c("HMIS 0-4",
         "CRVS 0-4",
         "CRVS 0-14",
         "CRVS 0-19",
         "CRVS 0-24")

dts_a5g <- 
  dts %>% 
  filter(Age %in% a5g) %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Age, Year, Source_type) %>% 
  unique() %>% 
  group_by(Country, Year, Source_type) %>% 
  mutate(n = n(),
         ages = case_when(n == 5 ~ "0-24",
                          n == 4 ~ "0-19",
                          n == 3 ~ "0-14",
                          n == 2 ~ "0-9",
                          n == 1 ~ "0-4"),
         ages = ifelse(ages == "0-24" & Source_type == "hmis", "0-4", ages),
         age_range = case_when(Source_type == "hmis" ~ paste0("HMIS ", ages),
                               Source_type == "crvs" ~ paste0("CRVS ", ages)),
         age_range = factor(age_range, levels = lvs))

unique(dts_a5g$age_range)
# yrs_a5g <- 
#   dts %>% 
#   filter(Age %in% a5g) %>% 
#   select(Country, Year) %>% 
#   filter(Year %in% 2020:2021) %>% 
#   unique() %>% 
#   mutate(id = 1) %>% 
#   spread(Year, id) %>% 
#   mutate(Year = case_when(`2020` == 1 & `2021` == 1 ~ "2020 and 2021",
#                           `2020` == 1 & is.na(`2021`) ~ "2020",
#                           is.na(`2020`) & `2021` == 1 ~ "2021"),
#          Year = factor(Year, levels = c("2020", "2021", "2020 and 2021"))) %>% 
#   rename(country = Country)


map_a5g <- 
  left_join(world_rob, dts_a5g,
            by = c('name' = 'Country'))


{
  cols <- 
    c("#34a0a4", "#168aad", "#1a759f", "#1e6091", "#184e77", 
      "#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a")
  cols <- 
    c("#d9ed92", "#168aad", "#1a759f", "#1e6091", "#184e77")
  tx <- 10


map_a5g %>% 
  ggplot() + 
  geom_sf(aes(fill = age_range), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = lvs) +
  # scale_fill_manual(values = cols, na.value = "white", breaks = c("2020", "2020 and 2021")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  labs(fill = "Years")+
  theme(legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.2),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

ggsave("figures/figS03_map_5y_ages_years.png", dpi = 700,
       w = 8,
       h = 4)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# income levels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income <- 
  dts %>% 
  select(country = Country, Income) %>% 
  unique()
unique(income$Income)

map_inc <- 
  left_join(world_rob, income,
            by = c('name' = 'country'))

tx <- 10

cols <- c("#d9ed92", "#76c893", "#168aad", "#184e77")
map_inc %>% 
  ggplot() + 
  geom_sf(aes(fill = Income), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  # scale_fill_manual(values = cols, na.value = "white") +
  scale_fill_manual(values = cols, na.value = "white", 
                    breaks = c("Low", "Lower-mid", "Upper-mid", "High")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent", size = 5)))+
  labs(fill = "Years")+
  theme(legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.2),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

ggsave("figures/figS04_map_income.png", dpi = 700,
       w = 8,
       h = 4)
