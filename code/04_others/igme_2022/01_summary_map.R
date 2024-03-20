source("Code/00_functions.R")

dts_yng <- read_rds("data_inter/annual_deaths_pop_5y_groups.rds")
dts_inf <- read_rds("data_inter/annual_deaths_pop_infant_child.rds")
rts_inf <- read_rds("Output/annual_rates_pop_infant_child.rds")

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

ggsave("Figures/last version/map_sources.png", dpi = 700,
       w = 5,
       h = 2.5)


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

ggsave("Figures/last version/map_infant.png", dpi = 700,
       w = 5,
       h = 2.5)
