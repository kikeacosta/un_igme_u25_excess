library(here)
source(here("Code", "00_functions.R"))

db_all_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds") %>% 
  filter(inc_in == 1)

lvs_cts_inf <- 
  db_all_fit %>% 
  filter(Year %in% 2020,
         Age == "Infant") %>% 
  arrange(-psc) %>% 
  pull(Country)

db_all_fit2 <- 
  db_all_fit %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Year, Country, Age, psc, lp, up) %>% 
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc))

db_all_fit_only_sig <- 
  db_all_fit2 %>% 
  select(-psc) %>% 
  rename(psc = psc_out)

pscs <- unique(db_all_fit_only_sig$psc) %>% sort


unique(db_all_fit_only_sig$Age) %>% sort()

# Map of excess mortality by age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                          TRUE ~ name)) %>% 
  filter(name != "Antarctica")

# add Robinson projection
world_rob <-
  st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
world_rob %>% ggplot() + geom_sf()

t1 <- bind_rows(world_rob %>% 
                  mutate(Year = 2020),
                world_rob %>% 
                  mutate(Year = 2021))
                


db_all_fit$Country[!db_all_fit$Country %in% world_rob$name]
tx <- 5
# db <- db_all_fit
plot_exc_map <- function(db, a = "15-19", wh = "all_ests"){
  # excl <- c("Armenia", "Mongolia")
  map_joined <- 
    t1 %>% 
    left_join(db %>% 
                filter(Age == a), 
              by = c('name' = 'Country', "Year"))
    
  map_joined %>% 
    ggplot() + 
    geom_sf(aes(fill = psc), col = "black", size = 0.1) +
    coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
    scale_fill_gradient2(
      low = "#3a86ff",
      mid = "white",
      high = "#e63946",
      midpoint = 1,
      space = "Lab",
      na.value = "grey70",
      guide = "colourbar",
      aesthetics = "fill"
    )+
    facet_grid(~Year)+
    labs(title = paste0(a, wh))+
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
  
  ggsave(paste0("Figures/maps/map_pscs_", a, wh, ".png"), dpi = 700, 
         width = 10, height = 3.5)
}

# as <- c("Infant", "0-4", "5-9", "10-14", "15-19", "20-24")
as <- unique(db_all_fit_only_sig$Age) %>% sort()
for(a in as){
  plot_exc_map(db_all_fit2, a, "_all_ests")
  plot_exc_map(db_all_fit_only_sig, a, "_incl_uncert")
}


