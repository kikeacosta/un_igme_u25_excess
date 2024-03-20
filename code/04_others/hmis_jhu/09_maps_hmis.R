source("Code/00_functions.R")

db_in <- 
  read_rds("Output/hmis_baselines_all_countries.rds") 

av_pscore <- 
  db_in %>% 
  filter(date >= "2020-03-01" & date <= "2020-12-31") %>% 
  mutate(pscore = value / bsn_gam) %>% 
  group_by(measure, country) %>% 
  summarise(av_psc = mean(pscore)) %>% 
  ungroup()

# pscores to send to UNICEF
out <- 
  db_in %>%
  select(-bsn_glm, -bsn_glm_l, -bsn_glm_u) %>% 
  rename(bsn = bsn_gam,
         bsn_l = bsn_gam_l,
         bsn_u = bsn_gam_u) %>% 
  mutate(pscore = value / bsn,
         pscore_l = value / bsn_l,
         pscore_u = value / bsn_u)
  
unique(out$measure)

write_csv(av_pscore, "Output/p_scores_2020_hmis_data.csv")
write_csv(out, "Output/p_scores_monthly_hmis_data.csv")



test <- 
  read_csv("Output/hmis_all_countries.csv") %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(pscore = value / bsn_gam) %>% 
  filter(measure %in% c("neo_lat"))


unique(av_pscore$measure)
unique(av_pscore$country)

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
  #                         name == "S. Sudan" ~ "South Sudan",
                          TRUE ~ name)) %>%
  filter(name != "Antarctica")

all_cts_wrld <- 
  World %>% 
  as_tibble() %>% 
  select(name) %>% 
  unique() 

# add Robinson projection
world_rob <-
  st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
world_rob %>% ggplot() + geom_sf()

ms <- c("sbs_r")

map_pscores <- function(ms = "stb_r"){

  db <- 
    av_pscore %>% 
    filter(measure %in% ms)
  
  # excl <- c("Armenia", "Mongolia")
  map_joined <- 
    world_rob %>% 
    left_join(db, 
              by = c('name' = 'country')) 
  
  cts_inc <- 
    db %>% 
    pull(country)
  
  world_points <- 
    st_centroid(map_joined)
  
  world_points <- 
    cbind(map_joined, st_coordinates(st_centroid(map_joined$geometry))) %>% 
    filter(name %in% cts_inc)
  
  tx <- 8
  out <- 
    map_joined %>% 
    ggplot() + 
    geom_sf(aes(fill = av_psc), col = "black", size = 0.1) +
    coord_sf(xlim = c(1.5e6, 9.5e6), ylim = c(-4e6, 4.5e6), expand = 0)+
    geom_text(data = world_points, 
              aes(x = X, y = Y, label = name),
              color = "black", 
              # fontface = "bold", 
              check_overlap = T,
              size = 1.5,
              vjust = 1)+
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
    labs(title = paste0(ms))+
    theme(legend.text = element_text(size = tx - 1),
          legend.title = element_text(size = tx),
          legend.key.size = unit(0.2, "cm"),
          legend.spacing = unit(c(0,0,0,0),"cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = c(0.6, 0.4),
          plot.title = element_text(size = tx + 3),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank())
  
  out
  
  ggsave(paste0("Figures/last version/hmis/map_p_scores_", ms, ".png"), 
         out, 
         dpi = 700, 
         width = 5, height = 5)
  
  return(out)
    
}

unique(av_pscore$measure)
map_pscores("neo_r")
map_pscores("sbs_r")
map_pscores("mat")
map_pscores("bts")
map_pscores("0_4")

ms <- unique(av_pscore$measure)

for(m in ms){
  map_pscores(m)
}

db <- 
  av_pscore %>% 
  filter(measure %in% ms)

# excl <- c("Armenia", "Mongolia")
map_joined <- 
  world_rob %>% 
  left_join(db, 
            by = c('name' = 'country')) 

unique(db$country)

cts_inc <- 
  db %>% 
  pull(country)

world_points <- 
  st_centroid(map_joined)
world_points <- 
  cbind(map_joined, st_coordinates(st_centroid(map_joined$geometry))) %>% 
  filter(name %in% cts_inc)


tx <- 8
map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = av_psc), col = "black", size = 0.1) +
  coord_sf(xlim = c(1.5e6, 9.5e6), ylim = c(-4e6, 4.5e6), expand = 0)+
  geom_text(data = world_points, 
            aes(x = X, y = Y, label = name),
            color = "black", 
            fontface = "bold", 
            check_overlap = T,
            size = 2,
            vjust = 1)+
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
  # facet_wrap(~ms)+
  labs(title = paste0(ms))+
  theme(legend.text = element_text(size = tx - 1),
        legend.title = element_text(size = tx),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.position = c(0.6, 0.4),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

