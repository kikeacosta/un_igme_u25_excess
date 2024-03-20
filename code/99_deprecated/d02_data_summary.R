rm (list = ls())
source("Code/00_functions.R")

# dts_inf <- read_rds("data_inter/annual_deaths_pop_infant_child.rds")
# dts_yng <- read_rds("data_inter/annual_deaths_pop_5y_groups.rds")
# rts_inf <- read_rds("data_inter/annual_rates_infant_child.rds")
# rts_yng <- read_rds("data_inter/annual_rates_5y_groups.rds")

# loading master deaths database
db_in <- read_rds("data_inter/annual_deaths_rates_2010_2021.rds")

unique(db_in$Source)
unique(db_in$Age)

# countries with data on births
birth_counts_cts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  pull(Country) %>% 
  unique() %>% 
  sort()

unique(db_in$type_data)

cts_rts <- 
  db_in %>% 
  filter(type_data == "rates") %>% 
  pull(Country) %>% 
  unique()

cts_bts <- c(birth_counts_cts, cts_rts) %>% sort


# table1 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all2 <- 
  db_in %>% 
  filter(Year %in% c(2020, 2021),
         Sex == "t") %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

write.excel(all2)

all3 <- 
  db_in %>% 
  filter(Year %in% c(2020, 2021),
         Sex == "t") %>% 
  select(Code, Country, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

write.excel(all3)

births <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year %in% c(2020, 2021)) %>% 
  select(Country, Year) %>% 
  mutate(bts = 1)
  
bts <- 
  db_in %>% 
  filter(Year %in% c(2020, 2021),
         Sex == "t") %>% 
  select(Code, Country, Year) %>% 
  unique() %>% 
  left_join(births) %>% 
  drop_na() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")

tot <- 
  db_in %>% 
  filter(Year %in% c(2020, 2021),
         Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")
  
bind_rows(tot,
          all2,
          bts,
          all3) %>% 
  select(var, everything())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts_dts_inf <- 
  unique(dts_inf$Country)

cts_rts_inf <- 
  unique(rts_inf$Country)

dts_all <- 
  dts_yng %>% 
  filter(!(Country %in% cts_dts_inf & Age == 0),
         !(Country %in% cts_exc)) %>% 
  bind_rows(dts_inf %>% 
              filter(!(Country %in% cts_exc)),
            rts_yng %>% 
              filter(!(Country %in% cts_rts_inf & Age == 0),
                     !(Country %in% cts_exc)),
            rts_inf) %>% 
  arrange(Country, Age, Sex, Year) %>% 
  # excluding total sex when there is data by sex
  group_by(Country, Code, Year, Age) %>% 
  filter(!(n() == 3 & Sex == "t")) %>% 
  ungroup() %>% 
  filter(Year >= 2015)
  
# # populations to exclude from the analyses
# excl <- c(
#   # too small population
#   "French Polynesia", 
#   "Seychelles", 
#   "Iceland", 
#   "Montenegro",
#   "Luxembourg",
#   "Malta",
#   "Cyprus",
#   # too poor age configuration
#   "Australia",
#   "Germany",
#   "China, Macao SAR",
#   "South Korea"
# )

# total deaths in 2020 by age (18.11.2021) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# total deaths for ages < 5
dts_all %>% 
  drop_na(Deaths) %>% 
  filter(age_up <= 0,
         Year >= 2020) %>% 
  summarise(Deaths = sum(Deaths))
# 176,723 under 1y

dts_all %>% 
  drop_na(Deaths) %>% 
  filter(age_up <= 4,
         Year >= 2020) %>% 
  summarise(Deaths = sum(Deaths))
# 232,828 under 5y

dts_all %>% 
  drop_na(Deaths) %>% 
  filter(age_up <= 24,
         Year >= 2020) %>% 
  summarise(Deaths = sum(Deaths))
# 496,071 under 25y


# summary of mortality data included ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

birth_data <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  mutate(bts = 1)

infant_data <- 
  dts_inf %>% 
  bind_rows(rts_inf) %>% 
  filter(Year >= 2020,
         Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  mutate(inf = 1)

yng_data <- 
  dts_yng %>% 
  bind_rows(rts_yng) %>% 
  filter(Year >= 2020,
         Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  mutate(yng = 1)
  
vr_summ <- 
  bind_rows(infant_data,
            yng_data) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  left_join(infant_data) %>% 
  left_join(yng_data) %>% 
  left_join(birth_data) %>% 
  replace_na(list(inf = 0, yng = 0, bts = 0)) %>% 
  mutate(tot = 1) %>% 
  group_by(Year) %>% 
  summarise(inf = sum(inf),
            yng = sum(yng),
            bts = sum(bts),
            tot = sum(tot)) %>% 
  ungroup() %>% 
  gather(-Year, key = measure, value = n) %>% 
  spread(Year, n) %>% 
  mutate(measure = factor(measure, levels = c("tot", "inf", "yng", "bts"))) %>% 
  arrange(measure) %>% 
  mutate(source = "VR")

vr_summ
write.excel(vr_summ)  
  

# HMIS data
# ~~~~~~~~~
hmis <- 
  read_rds("Output/hmis_all_countries.rds") %>% 
  filter(measure %in% c("neo", "inf", "sbs", "0_4", "bts")) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(year = year(date)) %>% 
  select(-value, -date) %>% 
  unique()

hmis2 <- 
  hmis %>% 
  select(-measure) %>% 
  unique() %>% 
  mutate(measure = "tot") %>% 
  bind_rows(hmis) %>% 
  group_by(measure, year) %>% 
  summarise(n = n()) %>% 
  spread(year, n) %>% 
  mutate(measure = factor(measure, levels = c("tot", "inf", "0_4", "bts", "sbs", "neo"))) %>% 
  arrange(measure) %>% 
  mutate(source = "HMIS")
    
hmis2
write.excel(hmis2)  


# Neonatal, Stillbirths in CCD






# ~~~~~~~~~~

sources <- 
  dts_all %>% 
  group_by(Country) %>% 
  summarise(Sources = paste0(unique(Source), collapse = "; ")) %>% 
  ungroup()

bts_yrs <- 
  birth_data %>% 
  group_by(Country) %>% 
  summarise(Births = unique(Year) %>% paste(collapse = ", "))

inf_yrs <- 
  infant_data %>% 
  group_by(Country) %>% 
  summarise(Infant = unique(Year) %>% paste(collapse = ", "))

available <- 
  dts_all %>% 
  # mutate(Year = as.integer(as.character(Year))) %>% 
  group_by(Country, Code, Year, Age) %>% 
  mutate(Sex_groups = n()) %>% 
  ungroup() %>% 
  group_by(Country, Code, Year, Sex) %>% 
  mutate(Age_groups = n(),
         Deaths = round(Deaths)) %>% 
  ungroup() %>% 
  mutate(Exposure = ifelse(is.na(Population), "no", "yes")) %>% 
  group_by(Country, Code) %>% 
  summarise(Age_groups = max(Age_groups),
            Sex_groups = max(Sex_groups),
            Period = paste(min(Year), max(Year), sep = "-"),
            Years = max(Year) - min(Year) + 1,
            Deaths = sum(Deaths),
            Exposure = unique(Exposure),
            Sources = unique(Source) %>% paste(collapse = ", "),
            Sources_pop = unique(Source_pop) %>% paste(collapse = ", ")) %>% 
  ungroup() %>% 
  left_join(inf_yrs) %>% 
  left_join(bts_yrs)

available <- 
  dts_all %>% 
  mutate(Year = as.integer(as.character(Year))) %>%
  group_by(Country, Code) %>% 
  summarise(Ages = paste0(min(Age), "-", max(age_up)),
            Period = paste(min(Year), max(Year), sep = "-"),
            Years = max(Year) - min(Year) + 1,
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  left_join(inf_yrs) %>% 
  left_join(bts_yrs) %>% 
  left_join(sources)

write.excel(available)


# # total deaths included in the analysis
# db %>% 
#   group_by(Country, Code, Year, Sex) %>% 
#   mutate(age_int_up = lead(Age) - 1) %>% 
#   ungroup() %>% 
#   filter(age_int_up < 25) %>% 
#   filter(!Country %in% excl) %>% 
#   summarise(Deaths = sum(Deaths))
# 
# # deaths in 2020 included in the analysis
# db %>% 
#   group_by(Country, Code, Year, Sex) %>% 
#   mutate(age_int_up = lead(Age) - 1) %>% 
#   ungroup() %>% 
#   filter(age_int_up < 25,
#          Year == 2020) %>% 
#   filter(!Country %in% excl) %>% 
#   summarise(Deaths = sum(Deaths))
# 
# available <- 
#   db %>% 
#   filter(age_up < 25, 
#          !Country %in% excl) %>% 
#   mutate(Year = as.integer(as.character(Year))) %>% 
#   group_by(Country, Code, Year, Source, Sex) %>% 
#   summarise(Age_groups = n(),
#             Deaths = sum(Deaths)) %>% 
#   ungroup() %>% 
#   group_by(Country) %>% 
#   mutate(Age_groups = min(Age_groups)) %>% 
#   ungroup() %>% 
#   group_by(Country, Code, Year, Source, Age_groups) %>% 
#   summarise(Sex_groups = n(),
#             Deaths = sum(Deaths)) %>% 
#   ungroup() %>% 
#   group_by(Country, Code, Source, Age_groups, Sex_groups) %>% 
#   summarise(min_year = min(Year),
#             max_year = max(Year),
#             Deaths = sum(Deaths)) %>% 
#   ungroup() %>% 
#   group_by(Country) %>% 
#   mutate(n_sources = n()) %>% 
#   ungroup() %>% 
#   group_by(Country, Code) %>% 
#   summarise(Age_groups = min(Age_groups),
#             Sex_groups = min(Sex_groups),
#             Period = paste(min(min_year), max(max_year), sep = "-"),
#             Years = max(max_year) - min(min_year) + 1,
#             Deaths = sum(Deaths), 
#             Sources = paste0(Source, collapse = "; "))
# 
# write.excel(available)

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
available2 <- 
  available %>% 
  mutate(Country = ifelse(Country %in% c("England and Wales", "Scotland", 
                                        "Northern Ireland"), 
                          "United Kingdom", 
                          Country),
         Code = ifelse(Country %in% c("United Kingdom"), 
                       "GBR", 
                       Code)) %>% 
  group_by(Country, Code, Infant, Births) %>% 
  unique() %>% 
  left_join(locs %>% 
              select(Code, Region, Development, geo_reg)) %>% 
  replace_na(list(Births = "No births data",
                  Infant = "No infant data"))


# unique(available3$Country)
# # sources
# srcs <- 
#   available3 %>% 
#   group_by(Sources) %>% 
#   summarise(n = n())
# 
# write.excel(srcs)
# 
# # age groups
# ages <- 
#   available3 %>% 
#   group_by(Age_groups) %>% 
#   summarise(N = n()) %>% 
#   group_by() %>% 
#   mutate(p = round(N / sum(N), 2))
# 
# write.excel(ages)
# 
# # periods
# pers <- 
#   available3 %>% 
#   group_by(Years) %>% 
#   summarise(N = n()) %>% 
#   group_by() %>% 
#   mutate(p = round(N / sum(N), 2))
# 
# 
# write.excel(pers)

# Regions
regs <- 
  available2 %>% 
  group_by(Region) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(regs_all) %>% 
  mutate(Coverage = N / Tot)

regs
write.excel(regs)

# Geo_regions
geo_regs <- 
  available2 %>% 
  group_by(geo_reg) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(geos_all) %>% 
  mutate(Coverage = N / Tot)

geo_regs
write.excel(geo_regs)


# development 
devs <- 
  available2 %>% 
  group_by(Development) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  left_join(devs_all) %>% 
  mutate(Coverage = N / Tot)

devs
write.excel(devs)

# save summary of data availability by country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(available2, "Output/01_summary_data_by_country.csv")


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
                          name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
                          TRUE ~ name)) %>% 
  filter(name != "Antarctica")

unique(World$name)
unique(available2$Country)
# add Robinson projection
world_rob <-
  st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# world_rob %>% ggplot() + geom_sf()

available2$Country[!available2$Country %in% world_rob$name]
available2$Code[!available2$Code %in% world_rob$iso_a3]

map_joined <- 
  left_join(world_rob, 
            available2, 
            by = c('name' = 'Country')) %>% 
  mutate(Years = ifelse(is.na(Years), 0, Years),
         Years = as.factor(Years))

cols <- c("grey", "#52b69a", "#34a0a4", "#168aad", "#1a759f", "#1e6091", "#184e77")
cols <- c("#E7B800", "#2E9FDF", "#FC4E07")
tx <- 8
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

# ggsave("Figures/data_summary/map_development.png", dpi = 700)


# infant mortality 
cols <- c("#264653", "#e9c46a", "#e76f51", "#2a9d8f")
tx <- 8
map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Infant), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  # scale_fill_manual(values = cols, na.value = "grey", breaks = c("no", "yes")) +
  scale_fill_manual(values = cols, na.value = "grey") +
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

ggsave("Figures/last version/map_infant_ages.png", dpi = 700)

cols <- c("#264653", "#e9c46a", "#e76f51", "#2a9d8f")
tx <- 8
map_joined %>% 
  ggplot() + 
  geom_sf(aes(fill = Births), col = "white", size = 0.2) +
  coord_sf(xlim = c(-13e6, 16e6), ylim = c(-6e6, 9e6), expand = 0)+
  # scale_fill_manual(values = cols, na.value = "grey", breaks = c("no", "yes")) +
  scale_fill_manual(values = cols, na.value = "grey") +
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

ggsave("Figures/last version/map_infant_ages.png", dpi = 700)
