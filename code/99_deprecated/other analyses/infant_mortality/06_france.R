library(here)
source(here("Code", "00_functions.R"))

file_names <- c(paste0("deces-", 2015:2020, ".txt"))


out <- list()
for(i in 2015:2020){
  out[[i]] <- 
    read_fwf(paste0("Data/France/deces-", i, ".txt"),
             fwf_widths(c(80, 1, 8, 5, 30 ,30, 8, 5, 9), 
                        c("Name", "Sex", "Date_birth", "Code_place_birth", 
                          "Place_birth", "Country", "Date_death", "Place_death", 
                          "Deaths_cert")),
             col_types = cols(.default = "c")) %>% 
    mutate(Year_record = i)
}

db_fra <- 
  out %>% 
  bind_rows() %>% 
  mutate(Date_birth = ymd(Date_birth),
         Date_death = ymd(Date_death))
