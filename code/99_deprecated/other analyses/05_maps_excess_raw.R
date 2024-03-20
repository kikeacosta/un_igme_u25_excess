library(here)
source(here("Code", "00_functions.R"))

db_all_fit <- 
  read_rds("Output/p_scores_excess_raw_data.rds")

lvs_cts_inf <- 
  db_all_fit %>% 
  filter(Year == 2020,
         Age == "Infant") %>% 
  arrange(-p_score) %>% 
  pull(Country)

db_all_fit2 <- 
  db_all_fit %>% 
  filter(Year == 2020) %>% 
  select(Country, Age, p_score, lp, up) %>% 
  mutate(p_score_out = ifelse(1 < up & 1 > lp, 1, p_score))

db_all_fit_only_sig <- 
  db_all_fit2 %>% 
  select(-p_score) %>% 
  rename(p_score = p_score_out)

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

db_all_fit$Country[!db_all_fit$Country %in% world_rob$name]
tx <- 5

plot_exc_map <- function(db, a = "15_24", wh = "all_ests"){
  excl <- c("Armenia", "Mongolia")
  map_joined <- 
    world_rob %>% 
    left_join(db %>% 
                filter(Age == a,
                       !Country %in% excl), 
              by = c('name' = 'Country')) 
    
  map_joined %>% 
    ggplot() + 
    geom_sf(aes(fill = p_score), col = "black", size = 0.1) +
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
  
  ggsave(paste0("Figures/excess_raw/map_p_scores_", a, wh, ".png"), dpi = 700, 
         width = 6, height = 3.5)
}

as <- c("Infant", "Child (0-4)", "5_9", "10_14", "15_24")

for(a in as){
  plot_exc_map(db_all_fit2, a, "_all_ests")
  plot_exc_map(db_all_fit_only_sig, a, "_incl_uncert")
}


