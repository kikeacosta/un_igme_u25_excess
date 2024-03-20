rm (list = ls())
source("Code/00_functions.R")

# data consolidation from deaths, exposures, and births

# consolidating deaths
source("Code/b01_consolidating_death_counts_data.R")
# consolidating rates
source("Code/b02_consolidating_death_rates_data.R")
# consolidating births
source("Code/c01_annual_births.R")
# annual population
source("Code/c02_annual_population.R")
# merging data
source("Code/d01_merging_deaths_rates_exposures.R")


