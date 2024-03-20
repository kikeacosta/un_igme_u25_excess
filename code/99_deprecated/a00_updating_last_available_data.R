rm (list = ls())
source("Code/00_functions.R")

# updating all available sources


# updating UNICEF country consultation data
ccd_file_name <- "Data/unicef/220712_Covid analysis data.xlsx"
source("Code/global_excess/a01_unicef_country_consultation_annual.R")

# stmf
source("Code/global_excess/a02_stmf.R")
# WHO
source("Code/global_excess/a03_who_mortality_database.R")
# UNPD
source("Code/global_excess/a05_unpd_data.R")
# HMD
source("Code/global_excess/a06_hmd_data.R")
# Eurostat
source("Code/global_excess/a07_eurs_data.R")

# # Brazil
# source("Code/global_excess/a08_brazil_data.R")
# # Peru
# source("Code/global_excess/a09_peru_data.R")
# # India
# source("Code/global_excess/a10_india_data.R")
