library(tidyverse)

fdts <- 
  read_fwf("Data/USA/Fetal/Fetal2020US_COD.txt" , 
           fwf_positions(c(90, 102, 331), c(90, 103, 332), 
                         c("age", "place", "weeks")),
           col_types = cols(.default = "c"))
           