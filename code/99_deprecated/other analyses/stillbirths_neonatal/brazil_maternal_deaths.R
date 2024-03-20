library(here)
source(here("Code", "00_functions.R"))
library(read.dbc)

# Maternal deaths
# ===============
# identify all files .dbc in directory
files <- list.files("Data/Brazil/maternal/", "dbc|DBC")
file_paths <- paste0("Data/Brazil/maternal/", files)
# read them all
mds <- list()
for(i in file_paths){
  mds[[i]] <- 
    read.dbc(i)
}

mds2 <- 
  mds %>% 
  bind_rows() %>% 
  as_tibble() 

mds3 <- 
  mds2 %>% 
  mutate(date = dmy(DTOBITO),
         year = year(date)) %>% 
  group_by(date, TPMORTEOCO, OBITOGRAV, OBITOPUERP) %>%
  summarise(dts = n())




