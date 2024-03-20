source("Code/00_functions.R")
library(cowplot)

dts_yng <- read_rds("data_inter/annual_deaths_pop_5y_groups.rds")
dts_inf <- read_rds("data_inter/annual_deaths_pop_infant_child.rds")
rts_inf <- read_rds("Output/annual_rates_pop_infant_child.rds")
sbs_neo <- read_rds("data_inter/neonatal_and_stillbirths_vital_reg.rds")
bts <- read_rds("data_inter/annual_births.rds")
hmis <- read_rds("Output/hmis_all_countries.rds")

cts_source <- 
  bind_rows(
    dts_yng %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    dts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    rts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    hmis %>% 
      select(Country = country) %>% 
      mutate(source = "HMIS")) %>% 
  mutate(Country = ifelse(Country %in% c("England and Wales", 
                                         "Scotland", 
                                         "Northern Ireland"), 
                          "United Kingdom", 
                          Country),
         source = factor(source, levels = c("VR", "HMIS"))) %>% 
  unique()


cts_inf <- 
  bind_rows(
    dts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    rts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    hmis %>% 
      filter(measure == "inf") %>% 
      select(Country = country) %>% 
      mutate(source = "HMIS")) %>% 
  mutate(Country = ifelse(Country %in% c("England and Wales", 
                                         "Scotland", 
                                         "Northern Ireland"), 
                          "United Kingdom", 
                          Country),
         infant = "yes") %>% 
  unique() %>% 
  full_join(cts_source) %>% 
  replace_na(list(infant = "no"))


cts_inf <- 
  bind_rows(
    dts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR"),
    rts_inf %>% 
      select(Country) %>% 
      mutate(source = "VR")) %>% 
  mutate(Country = ifelse(Country %in% c("England and Wales", 
                                         "Scotland", 
                                         "Northern Ireland"), 
                          "United Kingdom", 
                          Country),
         infant = "yes") %>% 
  unique() %>% 
  full_join(cts_source %>% filter(source == "VR")) %>% 
  replace_na(list(infant = "no"))





cts_hmis <- 
  hmis %>% 
  select(country) %>% 
  unique() %>% 
  mutate(avail = "y")


cts_sbs <- 
  sbs_neo %>% 
  filter(measure %in% c("sbs", "sbs_r")) %>% 
  select(country) %>% 
  unique() %>% 
  mutate(source = "VR") %>% 
  bind_rows(hmis %>% 
              select(country) %>% 
              unique() %>% 
              mutate(source = "HMIS"))



# Map of available data
library(cartography)
library(rgdal)
library(tmap)
library(sf)

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
                          name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
                          TRUE ~ name)) %>% 
  filter(name != "Antarctica")

# add Robinson projection
world_rob <-
  st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


unique(World$name)
unique(cts_source$Country)
unique(cts_inf$Country)

cts_source$Country[!cts_source$Country %in% world_rob$name]
cts_inf$Country[!cts_inf$Country %in% world_rob$name]


# data sources
# ~~~~~~~~~~~~
map_source <- 
  left_join(world_rob, cts_source, 
            by = c('name' = 'Country'))

cols <- c("#1d3557", "#e63946")
tx <- 6
map_source %>% 
  ggplot() + 
  geom_sf(aes(fill = source), col = "white", size = 0.1) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "grey", breaks = c("VR", "HMIS")) +
  theme(legend.text = element_text(size = tx + 1),
        legend.title = element_text(size = tx + 1),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.3),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

# ggsave("Figures/last version/map_sources.png", dpi = 700,
#        w = 5,
#        h = 2.5)


# infant deaths data 
# ~~~~~~~~~~~~~~~~~~
map_inf <- 
  left_join(world_rob, cts_inf, 
            by = c('name' = 'Country'))

cols <- c("#2a9d8f", "#1d3557")
tx <- 6
map_inf %>% 
  ggplot() + 
  geom_sf(aes(fill = infant), col = "white", size = 0.1) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "grey", breaks = c("yes", "no")) +
  theme(legend.text = element_text(size = tx + 1),
        legend.title = element_text(size = tx + 1),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.3),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

# ggsave("Figures/last version/map_infant_vr.png", dpi = 700,
#        w = 5,
#        h = 2.5)





# ~~~~~~~~~~~~~~~~~~~~~~~

map_hmis <- 
  left_join(world_rob, cts_hmis, 
            by = c('name' = 'country'))

cols <- c("#00b4d8", "#0077b6")
tx <- 6
map_hmis %>% 
  ggplot() + 
  geom_sf(aes(fill = avail), col = "white", size = 0.1) +
  coord_sf(xlim = c(-1.8e6, 9.5e6), ylim = c(-4e6, 4.5e6), expand = 0)+
  scale_fill_manual(values = "#0077b6", na.value = "grey",
                    guide = "none") +
  labs(fill = "")+
  theme(legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.3),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

# ggsave("Figures/last version/map_hmis.png", dpi = 700,
#        w = 2.5,
#        h = 2.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~

map_sbs <- 
  left_join(world_rob, cts_sbs,
            by = c('name' = 'country'))

cols <- c("#0077b6", "#00b4d8")
tx <- 6
map_sbs %>% 
  ggplot() + 
  geom_sf(aes(fill = source), col = "white", size = 0.1) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "grey", breaks = c("VR", "HMIS")) +
  labs(fill = "Source")+
  theme(legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.1, 0.3),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

# ggsave("Figures/last version/map_stillbirths.png", dpi = 700,
#        w = 5,
#        h = 2.5)

cts_sbs %>% 
  filter(source == "HMIS") %>% 
  pull(country) %>% length()

cts_sbs %>% 
  filter(source == "VR") %>% 
  pull(country) %>% length()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# map according to year of data availability
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs <- 
  bind_rows(dts_yng %>% select(Country, Year),
            dts_inf %>% select(Country, Year),
            rts_inf %>% select(Country, Year)) %>% 
  filter(Year %in% 2020:2021) %>% 
  unique() %>% 
  mutate(id = 1) %>% 
  spread(Year, id) %>% 
  mutate(Year = case_when(`2020` == 1 & `2021` == 1 ~ "2020 and 2021",
                          `2020` == 1 & is.na(`2021`) ~ "2020",
                          is.na(`2020`) & `2021` == 1 ~ "2021"),
         Year = factor(Year, levels = c("2020", "2021", "2020 and 2021"))) %>% 
  rename(country = Country)


map_yrs <- 
  left_join(world_rob, yrs,
            by = c('name' = 'country'))

cols <- c("#00b4d8", "#0077b6")
tx <- 10
map_yrs %>% 
  ggplot() + 
  geom_sf(aes(fill = Year), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = c("2020", "2020 and 2021")) +
  labs(fill = "Years")+
  theme(legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
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

ggsave("Figures/last version/manuscript/map_years.png", dpi = 700,
       w = 8,
       h = 4)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stillbirths and neonatal deaths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts_sbs_neo <- 
  sbs_neo %>% 
  filter(measure %in% c("neo", "sbs"),
         year %in% 2020:2021) %>%
  select(country, measure, year) %>% 
  unique() %>% 
  mutate(avail = "y") %>% 
  spread(measure, avail) %>% 
  mutate(both = ifelse(sbs == "y" & neo == "y", "y", NA),
         neo_only = ifelse(is.na(sbs) & neo == "y", "y", NA),
         sbs_only = ifelse(sbs == "y" & is.na(neo), "y", NA)) %>% 
  select(-neo, -sbs) %>% 
  gather(-country, -year, key = measure, value = avail) %>% 
  drop_na(avail) %>% 
  select(-avail)


map_sbs_neo_20 <- 
  left_join(world_rob, cts_sbs_neo %>% filter(year == 2020), 
            by = c('name' = 'country'))

map_sbs_neo_21 <- 
  left_join(world_rob, cts_sbs_neo %>% filter(year == 2021), 
            by = c('name' = 'country'))

cols <- c("#ee9b00", "#ae2012")
tx <- 10
sbs_neo_20 <- 
  map_sbs_neo_20 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = c("neo_only", "both"),
                    labels = c("Neonatal deaths",
                               "Neonatal deaths and Stillbirths")) +
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

sbs_neo_21 <- 
  map_sbs_neo_21 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = c("neo_only", "both"),
                    labels = c("Neonatal deaths",
                               "Neonatal deaths and Stillbirths")) +
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

for_leg <- 
  map_sbs_neo_20 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = c("neo_only", "both"),
                    labels = c("Neonatal deaths",
                               "Neonatal deaths and Stillbirths")) +
  theme(legend.text = element_text(size = tx),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
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
  plot_grid(sbs_neo_20, sbs_neo_21, 
            ncol = 2, 
            labels = c("2020", "2021"),
            hjust = 0,
            label_size = 10)

plot_grid(plots, legend, 
          ncol = 1, 
          rel_heights = c(1, 0.1))


ggsave("Figures/last version/manuscript/map_sbs_neo.png", dpi = 700,
       w = 8,
       h = 2)


cts_sbs_neo %>% 
  group_by(measure) %>% 
  summarise(n())



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# births and infant deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2020
inf_20 <- 
  bind_rows(dts_inf %>% 
              filter(Year %in% 2020) %>% 
              select(Country) %>% 
              unique(),
            rts_inf %>% 
              filter(Year %in% 2020) %>% 
              select(Country) %>% 
              unique()) %>% 
  mutate(inf = 1)

cts_inf_bts_20 <- 
  full_join(bts %>% 
              filter(Year %in% 2020) %>% 
              select(Country) %>% 
              unique() %>% 
              mutate(bts = 1),
            inf_20)  %>% 
  drop_na(inf) %>% 
  mutate(measure = case_when(inf == 1 & is.na(bts) ~ "inf",
                             inf == 1 & bts == 1 ~ "inf_bts",
                             TRUE ~ NA_character_)) %>% 
  select(country = Country, measure)

map_bts_inf_20 <- 
  left_join(world_rob, cts_inf_bts_20, 
            by = c('name' = 'country'))

# 2021
inf_21 <- 
  bind_rows(dts_inf %>% 
              filter(Year %in% 2021) %>% 
              select(Country) %>% 
              unique(),
            rts_inf %>% 
              filter(Year %in% 2021) %>% 
              select(Country) %>% 
              unique()) %>% 
  mutate(inf = 1)

cts_inf_bts_21 <- 
  full_join(bts %>% 
              filter(Year %in% 2021) %>% 
              select(Country) %>% 
              unique() %>% 
              mutate(bts = 1),
            inf_21)  %>% 
  drop_na(inf) %>% 
  mutate(measure = case_when(inf == 1 & is.na(bts) ~ "inf",
                             inf == 1 & bts == 1 ~ "inf_bts",
                             TRUE ~ NA_character_)) %>% 
  select(country = Country, measure)


# map
map_bts_inf_21 <- 
  left_join(world_rob, cts_inf_bts_21, 
            by = c('name' = 'country'))

cols <- c("#38b000", "#006400")
tx <- 10

m20 <-
  map_bts_inf_20 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white") +
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        plot.margin = margin(2, 0, 0, 0),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

m21 <- 
  map_bts_inf_21 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white") +
  theme(legend.position = "none",
        plot.title = element_text(size = tx + 3),
        plot.margin = margin(2, 0, 0, 0),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

for_leg <- 
  map_bts_inf_20 %>% 
  ggplot() + 
  geom_sf(aes(fill = measure), col = "grey", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "white", breaks = c("inf", "inf_bts"),
                    labels = c("Infant deaths",
                               "Infant deaths and Births")) +
  theme(legend.text = element_text(size = tx),
        legend.title = element_blank(),
        legend.key.size = unit(0.2, "cm"),
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
  plot_grid(m20, m21, 
            ncol = 2, 
            labels = c("2020", "2021"),
            hjust = 0,
            label_size = 10)

plot_grid(plots, legend, 
          ncol = 1, 
          rel_heights = c(1, 0.1))

ggsave("Figures/last version/manuscript/map_bts_inf.png", dpi = 700,
       w = 8,
       h = 2)
