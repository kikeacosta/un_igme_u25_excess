rm (list = ls()); gc()
source("code/00_functions.R")

## R wrapper to connect to DemoData (and optionally to DemoTools library)
## https://timriffe.github.io/DDSQLtools/
## https://timriffe.github.io/DDSQLtools/articles/Downloading-UNPD-data-into-R.html
## (optional) Tools for aggregate demographic analysis
## https://timriffe.github.io/DemoTools/

## Open API documentation for DemoData:
## https://popdiv.dfs.un.org//Demodata/swagger/ui/index#

## -----------------------------------------------------------------------
## install DemoTools and DDSQLtools (comments if already installed)
## devtools::install_github("timriffe/DemoTools", force=TRUE)
## -----------------------------------------------------------------------
## devtools::install_github("timriffe/DDSQLTools", force=TRUE)
## -----------------------------------------------------------------------

# List of packages for session
.packages = c("devtools", "data.table","tictoc","dplyr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


# devtools::install_github("timriffe/DDSQLTools", force=TRUE)
library(DDSQLtools)

options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
# options(scipen=999999)

## get_indicators():     Get information about available indicators (IndicatorID)
## get_iitypes():     Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
indicators <- data.table(get_iitypes())

indicators <- indicators[, .(IndicatorID=PK_IndicatorID, IndicatorName=Name, IndicatorShortName=ShortName, UnitShortLabel, VariableType, FormatString, ComponentID, ComponentName=IndicatorType.ComponentName, IndicatorTypeID, IndicatorTypeName=IndicatorType.Name, IsComplete, SortOrder)]
setorder(indicators, SortOrder)

unique(indicators$ComponentName)
indicators[ComponentName=="Population"]
indicators[ComponentName=="Fertility"]
indicators[ComponentName=="Mortality"]
indicators[ComponentName=="Life tables"]

## list of indicators related to Deaths by age and sex
indicators[IndicatorTypeName=="Deaths by age and sex"]

## Deaths by age and sex - abridged 
## Infant and child deaths by sex and age
indicators[IndicatorID %in% c(194, 314)]

## get_indicatortypes(): Get information about available indicators (IndicatorTypeID)

## get list of DataProcess
DataProcessType <- data.table(get_dataprocesstype())
DataProcess <- data.table(get_dataprocess())

DataProcess <- merge(DataProcess[, .(DataProcessID=PK_DataProcessID, DataProcessTypeID, Name, ShortName, SortOrder1)], DataProcessType[, .(PK_DataProcessTypeID, DataProcessTypeName=Name, DataProcessTypShortNamee=ShortName)], by.x="DataProcessTypeID", by.y="PK_DataProcessTypeID")

## example of selection for DataProcessTypeID
## 2=Census ; 11=Survey ; 12=Panel ; 8=PES
## 9=Register ; 7=Life Table (legacy UN DYB) ; 10=Sample Registration System (SRS)
## 6=Estimate
DataProcess[DataProcessTypeID %in% c(9), ]

## get list of locations from Server
Locations <- data.table(get_locations(addDefault = "false",
                                      includeDependencies = "false",
                                      includeFormerCountries = "false"))
Locations <- Locations[, .(LocID=PK_LocID, LocTypeID, LocName=Name)]

DataCatalog <- data.table(get_datacatalog(addDefault = "false"))
DataProcess <- data.table(get_dataprocess(addDefault = "false"))
DataProcessType <- data.table(get_dataprocesstype(addDefault = "false"))

DataCatalog <- subset(DataCatalog, select=c("DataCatalogID", "LocID", "LocTypeID", "LocName", "DataProcessTypeID", "DataProcessType", "DataProcessTypeShortName", "DataProcessID", "DataProcess", "DataProcessShortName", "Name", "ShortName", "OfficialName", "OfficialShortName", "ReferencePeriod", "ReferenceYearStart", "ReferenceYearEnd", "ReferenceYearMid", "FieldWorkStart", "FieldWorkEnd", "FieldWorkMiddle", "ParentDataCatalogID", "isSubnational"))
DataCatalog[, FieldWorkStart := as.Date(FieldWorkStart, format="%m/%d/%Y")]
DataCatalog[, FieldWorkEnd   := as.Date(FieldWorkEnd, format="%m/%d/%Y")]
DataCatalog <- DataCatalog[is.na(LocTypeID)==FALSE]
setorder(DataCatalog, LocName, ShortName, ReferenceYearStart)

## get list of all DataSources
all_ds <- data.table(get_datasources())
dyb_sd <- data.table(get_datasources(shortNames = "DYB"))
ipums_sd <- data.table(get_datasources(shortNames = "IPUMS"))

## sample of shortnames you can use to query only 1 data source:
# myDS <- "DYB"
# myDS <- "IPUMS"
## sample of shortnames you can use to query all data source:
myDS <- NULL

## example of test using Saudi Arabia, either using LocID or name
## myLocations <- c(682)
## myLocations <- "Saudi Arabia"

## example to query all locations in our DB
myLocations <- unique(Locations$LocID)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# mortality by age group ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here replace with number of desired chunk of countries
n_chunks <- 20
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# Loop through each location with `lapply`
# This is useful if you want to work with many locations because 
# the API can only handle a limited volume of queries at once.
myDT <- lapply(cnty_groups, function(x) {
  # Measure time of beginning
  tic()
  
  res <- get_recorddata(dataProcessTypeIds = c(9),    ## Register
                        startYear = 2010,
                        endYear = 2021,
                        indicatorIds = c(194),  ## Deaths by age and sex - abridged 
                        # isComplete = 1,       ## 0=Abridged or 1=Complete
                        locIds = x,             ## set of locations (M49/ISO3 numerical code or M49 names)
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
                        dataSourceShortNames = myDS,  ## default = NULL for all sources of information
                        includeUncertainty = FALSE,
                        collapse_id_name = FALSE)
  
  # Print time it took to make the request
  cat("Country", x, ":")
  toc()
  
  # return the result
  return(res)
})

# Merge all separate country data frames into one data frame.
DT <- data.table(do.call(rbind, myDT))

## compute number of records per series/sex
DT[, count := .N, by=list(SeriesID, SexID)]

# saving raw unpd deaths data 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_rds(DT, "data_inter/unpd_deaths_raw.rds",
          compress = "xz")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# neonatal mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~
 # Here replace with number of desired chunk of countries
n_chunks <- 20
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# Loop through each location with `lapply`
# This is useful if you want to work with many locations because 
# the API can only handle a limited volume of queries at once.
myDT <- lapply(cnty_groups, function(x) {
  # Measure time of beginning
  tic()
  
  res <- get_recorddata(dataProcessTypeIds = c(9),    ## Register
                        startYear = 2010,
                        endYear = 2021,
                        indicatorIds = c(221),  ## Neonatal mortality
                        locIds = x,             ## set of locations (M49/ISO3 numerical code or M49 names)
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
                        dataSourceShortNames = myDS,  ## default = NULL for all sources of information
                        includeUncertainty = FALSE,
                        collapse_id_name = FALSE)
  
  # Print time it took to make the request
  cat("Country", x, ":")
  toc()
  
  # return the result
  return(res)
})

# Merge all separate country data frames into one data frame.
DT <- data.table(do.call(rbind, myDT))

## compute number of records per series/sex
DT[, count := .N, by=list(SeriesID, SexID)]

# saving raw unpd stillbirths data 
write_rds(DT, "data_inter/unpd_neonatal_raw.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stillbirths ====
# ~~~~~~~~~~~~~~~~
# Here replace with number of desired chunk of countries
n_chunks <- 1
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# Loop through each location with `lapply`
# This is useful if you want to work with many locations because 
# the API can only handle a limited volume of queries at once.
myDT <- lapply(cnty_groups, function(x) {
  # Measure time of beginning
  tic()
  
  res <- get_recorddata(dataProcessTypeIds = c(9),    ## Register
                        startYear = 2010,
                        endYear = 2021,
                        indicatorIds = c(223),  ## Deaths by age and sex - abridged 
                        # isComplete = 1,       ## 0=Abridged or 1=Complete
                        locIds = x,             ## set of locations (M49/ISO3 numerical code or M49 names)
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
                        dataSourceShortNames = myDS,  ## default = NULL for all sources of information
                        includeUncertainty = FALSE,
                        collapse_id_name = FALSE)
  
  # Print time it took to make the request
  cat("Country", x, ":")
  toc()
  
  # return the result
  return(res)
})

# Merge all separate country data frames into one data frame.
DT <- data.table(do.call(rbind, myDT))

## compute number of records per series/sex
DT[, count := .N, by=list(SeriesID, SexID)]

# saving raw unpd stillbirths data 
write_rds(DT, "data_inter/unpd_stillbirths_raw.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# births ====
# ~~~~~~~~~~~
# Here replace with number of desired chunk of countries
# Here replace with number of desired chunk of countries
n_chunks <- 20
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# Loop through each location with `lapply`
# This is useful if you want to work with many locations because 
# the API can only handle a limited volume of queries at once.

myDT <- lapply(cnty_groups, function(x) {
  # Measure time of beginning
  tic()
  
  res <- get_recorddata(dataProcessTypeIds = c(9),    ## Register
                        startYear = 2010,
                        endYear = 2021,
                        indicatorIds = c(159),  ## Deaths by age and sex - abridged 
                        locIds = x,             ## set of locations (M49/ISO3 numerical code or M49 names)
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
                        dataSourceShortNames = myDS,  ## default = NULL for all sources of information
                        includeUncertainty = FALSE,
                        collapse_id_name = FALSE)
  
  # Print time it took to make the request
  cat("Country", x, ":")
  toc()
  
  # return the result
  return(res)
})

# Merge all separate country data frames into one data frame.
DT <- data.table(do.call(rbind, myDT))

## compute number of records per series/sex
DT[, count := .N, by=list(SeriesID, SexID)]

# saving raw unpd data 
# ~~~~~~~~~~~~~~~~~~~~
write_rds(DT, "data_inter/unpd_births_raw.rds")

