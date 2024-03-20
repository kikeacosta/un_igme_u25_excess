library(here)
source(here("Code", "00_functions.R"))

db <- read_rds("Output/annual_deaths_pop.rds")
exps <- read_rds("Output/exposures_single-years.rds")

# clist <- c("Brazil", "Chile", "Austria")
clist <- unique(db$Country) %>% sort()

c <- "Colombia"
s <- "f"
y <- "2019"
db_stm <- tibble()

for(c in clist){
  
  temp1 <- 
    db %>% 
    filter(Country == c)
  slist <- unique(temp1$Sex) %>% sort()
  
  for(s in slist){
    
    temp2 <- 
      temp1 %>% 
      filter(Sex == s) 
    
    ylist <- unique(temp2$Year) %>% sort()
    
    for(y in ylist){
      
      temp3 <- 
        temp2 %>% 
        filter(Year == y) %>% 
        arrange(Age) %>% 
        mutate(Age_mid = (Age + lead(Age))/2,
               Age_mid = ifelse(is.na(Age_mid), (Age + 105)/2, Age_mid))
        
      # smoothing using pclm with lambda 1e5, and including exposures 
      db_stm <- 
        db_stm %>% 
        bind_rows(smooth_mx(temp3, s, c, y, 1e4))
      
    }
  }
}

write_rds(db_stm, "Output/smoothed_mx_all_countries.rds")

