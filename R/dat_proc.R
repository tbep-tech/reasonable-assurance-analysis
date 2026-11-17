# setup ---------------------------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(lubridate)
library(here)
library(foreign)
library(dplyr)
library(smartsheetr)
library(readxl)
library(httr)
library(jsonlite)
library(sf)
library(tbeptools)
library(readxl)
library(haven)

source('R/funcs.R')

cenkey <- Sys.getenv("CENSUS_API_KEY")
smrkey <- Sys.getenv("SMARTSHEET_TOKEN")

census_api_key(cenkey)
#set_smartsheet_api_key(smrkey)

# Legacy annual loading data
#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/raw/tb_rasegsanntntph2o_8521.sas7bdat", 
#              destfile = here('data-raw/tb_rasegsanntntph2o_8521.sas7bdat'))

# Prior 2022 RA annual loading data
#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/raw/tbep2225.RData", 
#              destfile = here('data/tbep2225.RData'))

# Updated 2027 RA annual loading data
#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/raw/tbep2225.RData", 
#              destfile = here('data/tbep2225.RData'))

# DPS Corrections File data
#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/dpsupdate.RData", 
#              destfile = here('data/dpsupdate.RData'))

#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/raw/dps0420monthentbas.sas7bdat", 
#              destfile = here('data-raw/dps0420monthentbas.sas7bdat'))

#download.file("https://github.com/tbep-tech/load-estimates/raw/main/data/raw/dps1721monthentbas.sas7bdat", 
#              destfile = here('data-raw/dps1721monthentbas.sas7bdat'))

# population data ---------------------------------------------------------

##
# american community survey data (started in 2005)

# variables for acs (acs1, 3, or 5 are one, three, and five year results) from load_variables('2005', 'acs1')
# pop variable is B01003_001

yrs <- seq(2005, 2026)
out <- NULL
for(yr in yrs){
  
  tmp <- try(get_acs('metropolitan statistical area/micropolitan statistical area', variables = 'B01003_001', 
                     survey = 'acs1', year = yr, key = cenkey))
  
  if(inherits(tmp, 'try-error')){
    next
  }
  
  tmp <- tmp %>% 
    mutate(yr = !!yr)
  
  out <- bind_rows(out, tmp)
  
}

acsdat <- out %>% 
  filter(grepl('^Tampa', NAME)) %>% 
  select(yr, pop = estimate)

##
# from ES project, see https://github.com/tbep-tech/State-of-the-Bay/issues/19

legdat <- read.csv(here('data-raw/uscb_pop_estimates_tb_metro.csv')) %>% 
  select(
    yr = year, 
    pop = USCB_POP_EST_TB_Metro
  ) %>% 
  mutate(
    pop = gsub('\\,', '', pop),
    pop = as.numeric(pop)
  )

## 
# combine both
popdat <- bind_rows(legdat, acsdat) %>% 
  group_by(yr) %>% 
  summarise(pop = mean(pop), .groups = 'drop') %>% 
  arrange(yr)

##
# manually add 2020 (not in tidycensus yet)
# https://www.statista.com/statistics/815278/tampa-metro-area-population/

popdat <- popdat %>% 
  bind_rows(
    tibble(
      yr = c(2020), 
      pop = c(3187828))
  ) |> 
  arrange(yr)

save(popdat, file = here('data/popdat.RData'))


# copy 2023 lulc data -------------------------------------------------------------------------

#download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/lulc2023.RData", 
#              destfile = here('data/lulc2023.RData'))

# copy 2024 subtidal data ---------------------------------------------------------------------
 
#download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/sgdat2024.RData", 
#              destfile = here('data/sgdat2024.RData'))

# fluccs lookup table -------------------------------------------------------------------------

#fluccslkup <- read.dbf('T:/05_GIS/TBEP/LULC_PROCESSED/SWFWMD_2020_LULC_SEAGRASS_CLIP_MERGE_DISSOLVE_TB_BASINS.dbf') %>% 
#  select(FLUCCSCODE, LEV3) %>% 
#  unique

#save(fluccslkup, file = here('data/fluccslkup.RData'))

# BCBS, TCB, and MR chlorophyll data ----------------------------------------------------------

# stations here: https://drive.google.com/file/d/1sH9Jx1PQTVrvTaJ_FxDtf3IC-hIVzdpV/view

# from pinellas county water atlas, selected all stations that looked relevant for BCBS

bcbsseg <- st_read(here('data-raw/tampabay_ra_seg_watersheds.shp')) %>% 
  st_transform(crs = 4326) %>% 
  filter(BAY_SEGMEN == 5)

# original NNC report data, includes bcbs, tcb, mr
# G:\CONSORTIUM\EPA-FDEP Numeric Nutrient Criteria\2011 TBEP Consolidated Estuarine NNC Recommendations\TBEP Data\Water Quality Data\
chlnnc <- read_excel(here('data-raw/Pinellas Co. and Manatee Co. Data_segments5-7.xlsx'), na = '.')

# bcbs

bcbschlnnc <- chlnnc %>% 
  filter(Segment =='BCBS') %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    bay_segment = Segment,
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chlorophyll-a   (ugl)`, 
    Level
  ) %>% 
  mutate(
    SampleTime = ymd(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime),
    chla = as.numeric(chla)
  ) %>% 
  group_by(bay_segment, station, SampleTime, Latitude, Longitude, yr, mo) %>% 
  summarise(chla = mean(chla, na.rm = TRUE), .groups = 'drop') %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) 

# these are from S. Day, via email 9/6/22 for probabilistic
bcbschlraw1 <- read_excel(here('data-raw/Pinellas Co Boca Ciega data .xlsx'), sheet = '2003-2019')
bcbschlraw2 <- read_excel(here('data-raw/Pinellas Co Boca Ciega data .xlsx'), sheet = '2020')
bcbschlraw3 <- read_excel(here('data-raw/Pinellas Co Boca Ciega data .xlsx'), sheet = '2021')

bcbschl1 <- bcbschlraw1 %>% 
  select(
    station = Site, 
    SampleTime = Date_Text, 
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chl-a`, 
    chla_q = `Chla_q`, 
    Level
  ) %>% 
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime),
    chla = as.numeric(chla)
  ) %>% 
  filter(yr > 2010) %>% # nnc data is up to 2010
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL) %>% 
  group_by(bay_segment, station, SampleTime, Latitude, Longitude, yr, mo) %>% 
  summarise(chla = mean(chla, na.rm = TRUE), .groups = 'drop')

bcbschl2 <- bcbschlraw2 %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chl-a`, 
    chla_q = `Chla_q`, 
    Level
  ) %>% 
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime), 
    Latitude = as.numeric(Latitude), 
    Longitude = -1 * as.numeric(Longitude)
  ) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL) %>% 
  group_by(bay_segment, station, SampleTime, Latitude, Longitude, yr, mo) %>% 
  summarise(chla = mean(chla, na.rm = TRUE), .groups = 'drop')

bcbschl3 <- bcbschlraw3 %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chl-a`, 
    chla_q = `Chla_q`, 
    Level
  ) %>% 
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime), 
    Latitude = as.numeric(Latitude), 
    Longitude = -1 * as.numeric(Longitude)
  ) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL)  

bcbschl <- bind_rows(bcbschlnnc, bcbschl1, bcbschl2, bcbschl3)

# TCB - from Manatee County Water Atlas and original NNC report data
# data source/provider query is STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new) 

tcbchlnnc <- chlnnc %>% 
  filter(Segment == 'TCB') %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    bay_segment = Segment,
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chlorophyll-a   (ugl)`, 
    Level
  ) %>% 
  mutate(
    SampleTime = ymd(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime),
    chla = as.numeric(chla)
  ) %>% 
  group_by(bay_segment, station, SampleTime, Latitude, Longitude, yr, mo) %>% 
  summarise(chla = mean(chla, na.rm = TRUE), .groups = 'drop') %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  mutate(
    station = case_when(
      Latitude == 27.5273305 ~ '430', 
      Latitude == 27.5295 ~ 'TC3', 
      Latitude == 27.5478 ~ 'TC2', 
      Latitude == 27.5478333 ~ '408', 
      Latitude == 27.5527 ~ 'TC1', 
      Latitude == 27.5508861 ~ '395', 
      Latitude == 27.5526666 ~ '405'
    )
  ) 

# from water atlas
manchlraw <- read.csv(here('data-raw/manchl.txt'), sep = '\t', header = T)

tcbchl1 <- manchlraw %>%
  filter(WaterBodyName == 'Terra Ceia Bay') %>% 
  filter(Parameter %in% c('Chla_ugl', 'ChlaC_ugl')) %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'TCB',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  filter(yr > 2010) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) 

tcbchl <- bind_rows(tcbchlnnc, tcbchl1)

# MR - from Manatee County Water Atlas add MR and TCB data
# data source/provider query is STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new)

mrchlnnc <- chlnnc %>% 
  filter(Segment == 'MR') %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    bay_segment = Segment,
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chlorophyll-a   (ugl)`, 
    Level
  ) %>% 
  mutate(
    SampleTime = ymd(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime),
    chla = as.numeric(chla)
  ) %>% 
  group_by(bay_segment, station, SampleTime, Latitude, Longitude, yr, mo) %>% 
  summarise(chla = mean(chla, na.rm = TRUE), .groups = 'drop') %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  mutate(
    station = case_when(
      Latitude == 27.5209638 ~ '433', 
      Latitude == 27.5173333 ~ '434', 
      Latitude == 27.5173 ~ 'LM1', 
      Latitude == 27.5111972 ~ '431', 
      Latitude == 27.5033333 ~ '535', 
      Latitude == 27.5033 ~ 'LM2', 
      Latitude == 27.5056944 ~ '532'
    )
  ) %>% 
  filter(yr >= 1990 & yr <= 2010)

# water atlas  
mrchl1 <- manchlraw %>% 
  filter(WaterBodyName == 'Manatee River Estuary') %>% 
  filter(Parameter %in% c('Chla_ugl', 'ChlaC_ugl')) %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'MR',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  filter(yr > 2010) 

mrchl <- bind_rows(mrchlnnc, mrchl1)

# combine all
chldat <- epcdata %>% 
  rename(station = epchc_station) %>% 
  mutate(station = as.character(station)) %>% 
  select(matches(names(bcbschl))) %>% 
  bind_rows(bcbschl) %>% 
  bind_rows(tcbchl) %>% 
  bind_rows(mrchl)

save(chldat, file = here('data/chldat.RData'))


## Section corresponds to updated loading information for each major bay segment, inclusive of the Remainder of Lower Tampa Bay

# segment id, annuals
segidann <- tibble(
  BAY_SEG = c(1, 2, 3, 4, 5, 55, 6, 7, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay',
                  'Boca Ciega Bay', 'Boca Ciega Bay South', 'Terra Ceia Bay', 'Manatee River',
                  'Remainder Lower Tampa Bay')
)

# segment id, monthly
# 5 is all of BCB, 6 is Terra Ceia Bay, 7 is Manatee River, 55 is BCB south
# RA reports only BCB south so 5 is excluded here
segidmos <- tibble(
  bayseg = c(1, 2, 3, 4, 5, 55, 6, 7, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay',
                  'Boca Ciega Bay', 'Boca Ciega Bay South', 'Terra Ceia Bay', 'Manatee River',
                  'Remainder Lower Tampa Bay')
  )

segidall <- tibble(
  bayseg = c(1, 2, 3, 4, 5, 55, 6, 7, 5567),
  bay_segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay',
                  'Boca Ciega Bay', 'Boca Ciega Bay South', 'Terra Ceia Bay', 'Manatee River',
                  'Remainder Lower Tampa Bay')
  )

# Annual laoding data from 1985-2016
# hfc/city of tampa updated loading data
load(file = here('data/dpsupdate.RData'))

totanndatpre <- read_sas('data-raw/tb_rasegsanntntph2o_8521.sas7bdat') %>% 
  mutate(
    bay_segment = case_when(
      BAY_SEG == 1 ~ 'Old Tampa Bay', 
      BAY_SEG == 2 ~ 'Hillsborough Bay', 
      BAY_SEG == 3 ~ 'Middle Tampa Bay', 
      BAY_SEG == 4 ~ 'Lower Tampa Bay',
      BAY_SEG == 5 ~ 'Boca Ciega Bay',
      BAY_SEG == 55 ~ 'Boca Ciega Bay South',
      BAY_SEG == 6 ~ 'Terra Ceia Bay',
      BAY_SEG == 7 ~ 'Manatee River'
    )
  ) %>% 
  rename(
    year = YEAR, 
    tn_load = TN_tons, 
    tp_load = TP_tons, 
    hy_load = h2oload10e6m3
  ) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    tp_load = sum(tp_load, na.rm = T), 
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) %>% 
  filter(year < 2017)

# correction to totanndatpre from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = c('tn_load', 'tp_load', 'hy_load')) %>% 
  filter(year < 2017) %>% 
  select(-entity, -source)

rltbdat8516 <- totanndatpre %>% 
  mutate(
    bay_segment = case_when(
      bay_segment %in% c('Boca Ciega Bay South', 'Manatee River', 'Terra Ceia Bay') ~ 'Remainder Lower Tampa Bay',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(bay_segment))

# combine
allsegdat8516 <- totanndatpre %>% 
  bind_rows(rltbdat8516) %>%
  left_join(dpscorr, by = c('year', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    hy_load = ifelse(is.na(hy_load_diffv), hy_load, hy_load + hy_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -hy_load_diffv) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load), 
    tp_load = sum(tp_load), 
    hy_load = sum(hy_load), 
    .groups = 'drop'
  ) 

# Annual loading data from 2017-2021 (Corrected)
# 2017 - 2022 RA period (from updated file)
# source is here T:\03_BOARDS_COMMITTEES\05_TBNMC\2022_RA_Update\01_FUNDING_OUT\DELIVERABLES\TO-9\Loads1721_DocTables\Loads1721_DocTables
totanndat1721 <- read.csv(here('data-raw/totn1721_segsource.csv')) %>% 
  select(
    bay_segment = BAY_SEG, 
    year = Year, 
    source, 
    tn_load = tnload, 
    tp_load = tpload, 
    hy_load = h2oload10e6m3
  ) %>% 
  filter(bay_segment %in% c(1, 2, 3, 4, 6, 7, 55)) %>%
  mutate(
    bay_segment = as.character(factor(bay_segment, 
                                      levels = as.character(c(1, 2, 3, 4, 5, 55, 6, 7)), 
                                      labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Boca Ciega Bay South', 'Terra Ceia Bay', 'Manatee River')))
  ) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    tp_load = sum(tp_load, na.rm = T), 
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  )

# correction to totanndatos from hfc update
dpscorr <- dpsdiff_fun(dpsupdate, annual = T, total = T, varsel = c('tn_load', 'tp_load', 'hy_load')) %>% 
  filter(year > 2016 & year < 2022) %>% 
  select(-entity, -source)

totanndat1721 <- totanndat1721 %>% 
  left_join(dpscorr, by = c('year', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    hy_load = ifelse(is.na(hy_load_diffv), hy_load, hy_load + hy_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -hy_load_diffv)

rltbdat1721 <- totanndat1721 %>% 
  mutate(
    bay_segment = case_when(
      bay_segment %in% c('Boca Ciega Bay South', 'Manatee River', 'Terra Ceia Bay') ~ 'Remainder Lower Tampa Bay',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(bay_segment))

# combine
allsegdat1721 <- totanndat1721 %>% 
  bind_rows(rltbdat1721) %>%
  left_join(dpscorr, by = c('year', 'bay_segment')) %>% 
  mutate(
    tn_load = ifelse(is.na(tn_load_diffv), tn_load, tn_load + tn_load_diffv),
    tp_load = ifelse(is.na(tp_load_diffv), tp_load, tp_load + tp_load_diffv),
    hy_load = ifelse(is.na(hy_load_diffv), hy_load, hy_load + hy_load_diffv)
  ) %>% 
  select(-tn_load_diffv, -tp_load_diffv, -hy_load_diffv) %>% 
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load), 
    tp_load = sum(tp_load), 
    hy_load = sum(hy_load), 
    .groups = 'drop'
  ) 


# 2022 - 2025, TBEP-derived (replaces RP's TOTLoadsRASeg2224.csv deliverable; see
# tbeploadsproc/R/09_export_2225.R)

load(file = here('data/tbep2225.RData'))

totanndattbep2225 <- bind_rows(ad2225seg_yr, dps2225seg_yr, gw2225seg_yr, ips2225seg_yr, ml2225seg_yr, nps2225seg_yr, spr2225seg_yr) %>%
  left_join(segidall, by = c('segment' = 'bay_segment')) %>%
  left_join(segidmos, by = 'bayseg') %>%
  filter(!is.na(bay_segment)) %>%
  group_by(year = Year, bay_segment) %>%
  summarise(
    tn_load = sum(tn_load, na.rm = T),
    tp_load = sum(tp_load, na.rm = T),
    hy_load = sum(hy_load, na.rm = T),
    .groups = 'drop'
  ) 

rltbdattbep2225 <- totanndattbep2225 %>% 
  mutate(
    bay_segment = case_when(
      bay_segment %in% c('Boca Ciega Bay South', 'Manatee River', 'Terra Ceia Bay') ~ 'Remainder Lower Tampa Bay',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(bay_segment))

# combine
allsegdattbep2225 <- totanndattbep2225 %>% 
  bind_rows(rltbdattbep2225) %>%
  group_by(year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load), 
    tp_load = sum(tp_load), 
    hy_load = sum(hy_load), 
    .groups = 'drop'
  ) %>%
  filter(bay_segment != "Boca Ciega Bay")
  

# Combine all years

allsegdat8525 <- allsegdat8516 %>%
  bind_rows(allsegdat1721) %>%
  bind_rows(allsegdattbep2225) %>%
  mutate(
    tnhy = tn_load / hy_load, 
    tphy = tp_load / hy_load,
    bay_segment = factor(bay_segment, 
                         levels = c("Old Tampa Bay", "Hillsborough Bay", "Middle Tampa Bay", "Lower Tampa Bay", "Remainder Lower Tampa Bay", "Boca Ciega Bay South", "Terra Ceia Bay", "Manatee River"))
  )

save(allsegdat8525, file = here('data/allsegdat8526.RData'))
