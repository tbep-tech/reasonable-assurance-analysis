library(here)
library(foreign)
library(dplyr)
library(sf)
library(lubridate)
library(tbeptools)
library(readxl)

# copy 2020 lulc data -------------------------------------------------------------------------

download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/lulc2020.RData", 
              destfile = here('data/lulc2020.RData'))

# copy 2020 subtidal data ---------------------------------------------------------------------
 
download.file("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/sgdat2020.RData", 
              destfile = here('data/sgdat2020.RData'))

# fluccs lookup table -------------------------------------------------------------------------

fluccslkup <- read.dbf('T:/05_GIS/TBEP/LULC_PROCESSED/SWFWMD_2020_LULC_SEAGRASS_CLIP_MERGE_DISSOLVE_TB_BASINS.dbf') %>% 
  select(FLUCCSCODE, LEV3) %>% 
  unique

save(fluccslkup, file = here('data/fluccslkup.RData'))

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
