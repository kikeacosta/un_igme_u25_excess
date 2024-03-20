rm (list = ls())
source("code/00_functions.R")

dts <- 
  read_rds("data_output/preliminary_pscores_2024-01-16.rds")

unique(dts$Country)

dts2 <- 
  dts %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1,
         Age != "0-4") %>% 
  select(country = Country, code = Code,
         year = Year, age = Age, 
         pop = Exposure,
         dts = Deaths, bsn, bsn_up, bsn_lp, psc, up, lp) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Excess",
                         up < 1 & lp < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, 
                      levels = c("Excess", "No-change", "Deficit")),
         year = year %>% as.character())

unique(dts2$age)
unique(dts2$country)

# extreme p-score values
# ~~~~~~~~~~~~~~~~~~~~~~

dts2 %>% 
  group_by(age, year) %>% 
  summarise(min_psc = min(psc) - 1,
            max_psc = max(psc) - 1)

dts3 <- 
  dts2 %>% 
  mutate(psc2 = ifelse(exc == "No-change", 0, 100*(psc - 1)),
         psc_pos = ifelse(psc2 > 0, psc2, NA),
         psc_neg = ifelse(psc2 < 0, psc2, NA)) 

qtls <- 
  dts3 %>% 
  filter(exc != "No-change") %>% 
  mutate(
    p_q1 = case_when(psc2 > 0 ~ quantile(psc_pos, prob = 0, na.rm = TRUE),
                     psc2 < 0 ~ quantile(psc_neg, prob = 0, na.rm = TRUE),
                     TRUE ~ 0),
    p_q2 = case_when(psc2 > 0 ~ quantile(psc_pos, prob = 0.33, na.rm = TRUE),
                     psc2 < 0 ~ quantile(psc_neg, prob = 0.33, na.rm = TRUE),
                     TRUE ~ 0),
    p_q3 = case_when(psc2 > 0 ~ quantile(psc_pos, prob = 0.66, na.rm = TRUE),
                     psc2 < 0 ~ quantile(psc_neg, prob = 0.66, na.rm = TRUE),
                     TRUE ~ 0),
    p_q4 = case_when(psc2 > 0 ~ quantile(psc_pos, prob = 1, na.rm = TRUE),
                     psc2 < 0 ~ quantile(psc_neg, prob = 1, na.rm = TRUE),
                     TRUE ~ 0),
    .by = c(age)
  ) %>% 
  select(country, code, year, age, psc, psc2, exc,
         starts_with("p_")) %>% 
  group_by(year, age, exc) %>% 
  filter(n() > 2) %>% 
  mutate(
    psc_rnk = cut(psc2, 
                  breaks = c(unique(p_q1), 
                             unique(p_q2), 
                             unique(p_q3), 
                             unique(p_q4)), 
                  labels = F,
                  # dig.lab = 2,
                  include.lowest = T),
  ) %>% 
  ungroup() %>% 
  select(country, code, year, age, psc, psc2, exc, 
         psc_rnk,
         starts_with("p_")
         )

cuts2 <- 
  qtls %>% 
  select(age, exc, psc_rnk, starts_with("p_")) %>% 
  unique() %>% 
  arrange(age, exc, psc_rnk) %>% 
  # mutate(p_low = case_when(psc_rnk == 1 & exc))
  mutate(
    psc_cut = case_when(
      exc == "Deficit" & psc_rnk == 3 ~ paste0("(", 
                                               round(p_q4), 
                                               ",", 
                                               round(p_q3),
                                               "]"),
      exc == "Deficit" & psc_rnk == 2 ~ paste0("(", 
                                               round(p_q3), 
                                               ",", 
                                               round(p_q2),
                                               "]"),
      exc == "Deficit" & psc_rnk == 1 ~ paste0("<", 
                                               round(p_q1)),
      
      
      exc == "Excess" & psc_rnk == 1 ~ paste0("(", 
                                              round(p_q1), 
                                              ",", 
                                              round(p_q2),
                                              "]"),
      exc == "Excess" & psc_rnk == 2 ~ paste0("(", 
                                              round(p_q2), 
                                              ",", 
                                              round(p_q3),
                                              "]"),
      exc == "Excess" & psc_rnk == 3 ~ paste0(">", 
                                              round(p_q3)),
      TRUE ~ "0"
    )
  ) %>% 
  select(age, exc, psc_rnk, psc_cut)

qtls2 <- 
  qtls %>% 
  select(-starts_with("p_")) %>% 
  left_join(cuts2)


col_neg <- colorRampPalette(c("#70E7FF", "#0091AD"), space = "Lab")(3)
col_net <- c("#A69AC1")
col_pos <- colorRampPalette(c("#F977A8", "#b7094c"), space = "Lab")(3)

dt_mp <- 
  dts3 %>% 
  select(country, code, year, age, psc, exc) %>% 
  left_join(qtls2) %>% 
  mutate(psc2 = ifelse(exc == "No-change", 0, psc2),
         psc_rnk = ifelse(exc == "No-change", 1, psc_rnk),
         psc_cut = ifelse(exc == "No-change", 0, psc_cut),
         comp = paste(exc, psc_rnk, sep = "-")) %>% 
  mutate(comp = factor(comp, levels = c("Deficit-1",
                                        "Deficit-2",
                                        "Deficit-3",
                                        "No-change-1",
                                        "Excess-1",
                                        "Excess-2",
                                        "Excess-3")))

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


ag <- "20-24"
yr <- 2021
ag <- "Stillbirths"

map_this <- function(ag = "20-24"){
  
  cols <- 
    c("Excess-1" = col_pos[1],
      "Excess-2" = col_pos[2],
      "Excess-3" = col_pos[3],
      "Deficit-1" = col_neg[3],
      "Deficit-2" = col_neg[2],
      "Deficit-3" = col_neg[1],
      "No-change-1" = col_net)
  
  chunk <- 
    dt_mp %>% 
    filter(age == ag)
  
  lbs <- 
    chunk %>% 
    select(comp, psc_cut) %>% 
    unique() %>% 
    arrange(comp) %>% 
    pull(psc_cut)
  
  tx <- 10
 
  world_rob2 <- 
    bind_rows(world_rob %>% select(country = name, geometry) %>% mutate(year = "2020"),
              world_rob %>% select(country = name, geometry) %>% mutate(year = "2021")) %>% 
    left_join(chunk)
  
  world_rob2 %>%
    ggplot() + 
    geom_sf(aes(fill = comp), col = "grey40", size = 0.1) +
    coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
    scale_fill_manual(values = cols, 
                      na.value = "transparent",
                      labels = lbs
    )+
    guides(fill = guide_legend(override.aes = list(colour = "transparent", 
                                                   size = 5),
                               nrow = 1))+
    facet_wrap(~year)+
    labs(fill = "Change (%)", title = ag) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.text = element_text(size = tx),
      legend.title = element_text(size = tx),
      legend.key.size = unit(0.2, "cm"),
      legend.key = element_rect(colour = "transparent",
                                fill = "transparent"),
      legend.spacing = unit(c(0,0,0,0),"cm"),
      legend.margin = margin(0, 0, 0, 0),
      # legend.position = c(0.1, 0.2),
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent"),
      plot.title = element_text(size = tx + 1),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank()
    )
}

map_this("Stillbirths")
ggsave("figures/01_manuscript/figS10_map_psc_sbs.png", dpi = 700,
       w = 8,
       h = 3)

map_this("Neonatal")
ggsave("figures/01_manuscript/figS11_map_psc_neo.png", dpi = 700,
       w = 8,
       h = 3)

map_this("Infant")
ggsave("figures/01_manuscript/figS12_map_psc_inf.png", dpi = 700,
       w = 8,
       h = 3)

map_this("1-4")
ggsave("figures/01_manuscript/figS13_map_psc_1-4.png", dpi = 700,
       w = 8,
       h = 3)

map_this("5-9")
ggsave("figures/01_manuscript/figS14_map_psc_5-9.png", dpi = 700,
       w = 8,
       h = 3)

map_this("10-14")
ggsave("figures/01_manuscript/figS15_map_psc_10-14.png", dpi = 700,
       w = 8,
       h = 3)

map_this("15-19")
ggsave("figures/01_manuscript/figS16_map_psc_15-19.png", dpi = 700,
       w = 8,
       h = 3)

map_this("20-24")
ggsave("figures/01_manuscript/figS17_map_psc_20-24.png", dpi = 700,
       w = 8,
       h = 3)



