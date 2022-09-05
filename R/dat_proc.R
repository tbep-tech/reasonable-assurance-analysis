library(here)
library(foreign)
library(dplyr)
library(sf)
library(lubridate)
library(tbeptools)

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

# from pinellas county water atlas, selected all stations that looked relevant for BCBS

bcbsseg <- st_read(here('data-raw/tampabay_ra_seg_watersheds.shp')) %>% 
  st_transform(crs = 4326) %>% 
  filter(BAY_SEGMEN == 5)

pinchlraw <- read.csv(here('data-raw/pinchl.txt'), sep = '\t', header = T)

pinchl <- pinchlraw %>% 
  filter(Parameter == 'Chla_ugl') %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'BCBS',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  select(bay_segment, epchc_station, SampleTime, yr, mo, everything()) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL)

# add MR and TCB data

chldat <- epcdata %>% 
  rename(station = epchc_station) %>% 
  mutate(station = as.character(station)) %>% 
  select(matches(names(pinchl))) %>% 
  bind_rows(pinchl)

save(chldat, file = here('data/chldat.RData'))
