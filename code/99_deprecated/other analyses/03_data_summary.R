library(here)
source(here("Code", "00_functions.R"))

db <- read_rds("Output/annual_deaths_pop.rds")

# total deaths for ages < 5
infs <- 
  db %>% 
  filter(Age < 25,
         Year == 2020) %>% 
  summarise(Deaths = sum(Deaths))

# total deaths in 2020 by age (23.08.2021) 
# 392,356 under 5 years
# 978,495 under 25 years

available <- 
  db %>% 
  filter(Age < 25) %>% 
  mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Country, Code, Year, Source, Sex) %>% 
  summarise(Age_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(Age_groups = min(Age_groups)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Year, Source, Age_groups) %>% 
  summarise(Sex_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Source, Age_groups, Sex_groups) %>% 
  summarise(min_year = min(Year),
            max_year = max(Year),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(n_sources = n()) %>% 
  ungroup() 

available2 <- 
  available %>% 
  group_by(Country, Code) %>% 
  summarise(Age_groups = min(Age_groups),
            Sex_groups = min(Sex_groups),
            Period = paste(min(min_year), max(max_year), sep = "-"),
            Years = max(max_year) - min(min_year) + 1,
            Deaths = sum(Deaths), 
            Sources = paste0(Source, collapse = "; "))



# populations to exclude from the analyses
excl <- c(
  # too small population
  "French Polynesia", 
  "Seychelles", 
  "Iceland", 
  "Montenegro",
  "Luxembourg",
  "Malta",
  "Cyprus",
  # too poor age configuration
  "Australia",
  "Germany",
  "China, Macao SAR",
  "South Korea"
)

# total deaths included in the analysis
db %>% 
  filter(Age < 25) %>% 
  filter(!Country %in% excl) %>% 
  summarise(Deaths = sum(Deaths))

# deaths in 2020 included in the analysis
db %>% 
  filter(Age < 25,
         Year == 2020) %>% 
  filter(!Country %in% excl) %>% 
  summarise(Deaths = sum(Deaths))

available <- 
  db %>% 
  filter(Age < 25,
         !Country %in% excl) %>% 
  mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Country, Code, Year, Source, Sex) %>% 
  summarise(Age_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(Age_groups = min(Age_groups)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Year, Source, Age_groups) %>% 
  summarise(Sex_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Source, Age_groups, Sex_groups) %>% 
  summarise(min_year = min(Year),
            max_year = max(Year),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(n_sources = n()) %>% 
  ungroup() %>% 
  group_by(Country, Code) %>% 
  summarise(Age_groups = min(Age_groups),
            Sex_groups = min(Sex_groups),
            Period = paste(min(min_year), max(max_year), sep = "-"),
            Years = max(max_year) - min(min_year) + 1,
            Deaths = sum(Deaths), 
            Sources = paste0(Source, collapse = "; "))

write.excel(available)

# loading locations from WPP documentation
# https://population.un.org/wpp/Download/Metadata/Documentation/
locs <- read_xlsx(here("Data", "WPP2019_F01_LOCATIONS.xlsx"),
                   skip = 16) %>% 
  select(Country = 2, 
         Code = 5,
         Region = 15, 
         geo_reg = 17, 
         most_dev = 18,
         least_dev = 20,
         mid_dev = 21) %>% 
  filter(!is.na(Code)) %>% 
  mutate(Development = case_when(!is.na(most_dev) ~ "most",
                         !is.na(least_dev) ~ "least",
                         !is.na(mid_dev) ~ "mid")) %>% 
  select(-most_dev, least_dev, mid_dev)

regs_all <- 
  locs %>% 
  group_by(Region) %>% 
  summarise(Tot = n())

geos_all <- 
  locs %>% 
  group_by(geo_reg) %>% 
  summarise(Tot = n())

devs_all <- 
  locs %>% 
  group_by(Development) %>% 
  summarise(Tot = n())


    
# merging locations with data
available3 <- 
  available2 %>% 
  left_join(locs %>% 
              select(Code, Region, Development, geo_reg))


unique(available3$Country)
# sources
srcs <- 
  available3 %>% 
  group_by(Sources) %>% 
  summarise(n = n())



write.excel(srcs)

# age groups
ages <- 
  available3 %>% 
  group_by(Age_groups) %>% 
  summarise(N = n()) %>% 
  group_by() %>% 
  mutate(p = round(N / sum(N), 2))

write.excel(ages)

# periods
pers <- 
  available3 %>% 
  group_by(Years) %>% 
  summarise(N = n()) %>% 
  group_by() %>% 
  mutate(p = round(N / sum(N), 2))


write.excel(pers)

# Regions
regs <- 
  available3 %>% 
  group_by(Region) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(regs_all) %>% 
  mutate(Coverage = N / Tot)

write.excel(regs)

# Geo_regions
geo_regs <- 
  available3 %>% 
  group_by(geo_reg) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(geos_all) %>% 
  mutate(Coverage = N / Tot)

write.excel(geo_regs)


# development 
devs <- 
  available3 %>% 
  group_by(Development) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(devs_all) %>% 
  mutate(Coverage = N / Tot)

write.excel(devs)


# data on young and infant mortality
available_young <- 
  db %>% 
  mutate(Year = as.integer(as.character(Year)),
         Young = ifelse(Age < 25, 1, 0)) %>% 
  arrange(Country, Code, Year, Sex, Age) %>% 
  group_by(Country, Code, Year, Sex, Source) %>% 
  mutate(Infant = ifelse(Age == 0 & lead(Age) == 1, "yes", "no")) %>% 
  filter(Young == 1) %>% 
  summarise(Age_groups = n(),
            Infant = max(Infant),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Year, Source, Age_groups, Infant) %>% 
  summarise(Sex_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Source, Age_groups, Sex_groups, Infant) %>% 
  summarise(min_year = min(Year),
            max_year = max(Year),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(n_sources = n()) %>% 
  ungroup() 

available_young2 <- 
  available_young %>% 
  group_by(Country, Code) %>% 
  summarise(Age_groups = min(Age_groups),
            Sex_groups = min(Sex_groups),
            Period = paste(min(min_year), max(max_year), sep = "-"),
            Years = max(max_year) - min(min_year) + 1,
            Deaths = sum(Deaths), 
            Sources = paste0(Source, collapse = "; "),
            Infant = paste0(Infant, collapse = "; ")) %>% 
  ungroup() %>% 
  mutate(Infant = ifelse(Infant == "yes", "yes", "no"))

# age groups
ages <- 
  available_young2 %>% 
  group_by(Age_groups) %>% 
  summarise(N = n())

write.excel(ages)

# age groups
inft <- 
  available_young2 %>%
  group_by(Infant) %>% 
  summarise(N = n()) %>% 
  group_by() %>% 
  mutate(p = round(N / sum(N), 2))


write.excel(inft)


# save summary of data availability by country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(available3, "Output/01_summary_data_by_country.csv")


# Map of data coverage
# ~~~~~~~~~~~~~~~~~~~~

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
world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
world_rob %>% ggplot() + geom_sf()

available3$Country[!available3$Country %in% world_rob$name]
available3$Code[!available3$Code %in% world_rob$iso_a3]

map_joined <- 
  left_join(world_rob, available3, 
                        by = c('name' = 'Country')) %>% 
  mutate(Years = ifelse(is.na(Years), 0, Years),
         Years = as.factor(Years))

tx <- 5

cols <- c("grey", "#d9ed92", "#99d98c", "#52b69a", "#1e6091", "#1e6091", "#184e77")

map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Years), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols) +
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

ggsave("Figures/data_summary/map_years.png", dpi = 700)

# 'development' level
cols <- c("grey", "#52b69a", "#34a0a4", "#168aad", "#1a759f", "#1e6091", "#184e77")
cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Development), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "grey", 
                    breaks = c("least", "mid", "most")) +
  theme(legend.text = element_text(size = tx + 1),
        legend.title = element_text(size = tx + 1),
        legend.key.size = unit(0.2, "cm"),
        legend.spacing = unit(c(0,0,0,0),"cm"),
        legend.margin = margin(0, 0, 0, 0),
        # legend.position = c(0.1, 0.3),
        legend.position = c(0.7, 0.2),
        plot.title = element_text(size = tx + 3),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

ggsave("Figures/data_summary/map_development.png", dpi = 700)



# Age groups at young ages 
map_joined <- 
  left_join(world_rob, available_young2, 
            by = c('name' = 'Country')) %>% 
  mutate(Age_groups = ifelse(is.na(Age_groups), 0, Age_groups),
         Age_groups = as.factor(Age_groups))

cols <- c("grey", "#d9ed92", "#b5e48c", "#52b69a", "#34a0a4", "#184e77")

map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Age_groups), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols) +
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

ggsave("Figures/data_summary/map_young_ages.png", dpi = 700)


# infant mortality 
cols <- c("#b5e48c", "#184e77")

map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Infant), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  scale_fill_manual(values = cols, na.value = "grey", breaks = c("no", "yes")) +
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

ggsave("Figures/data_summary/map_infant_ages.png", dpi = 700)

