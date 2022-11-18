library(tidyverse)
library(here)
library(tibble)
library(tbeptools)
library(flextable)
library(haven)

# tn v hy, tp v hy all segs as table ----------------------------------------------------------

# ref lines, all from 2009 Reasonal Assurance Addendum (except tphy, from RP email 11/4/22)
# https://drive.google.com/file/d/10IjJAfcGFf007a5VdPXAUtUi4dx-cmsA/view
reflns <- data.frame(
  bay_segment = c("Old Tampa Bay", "Hillsborough Bay", "Middle Tampa Bay", "Lower Tampa Bay", "Remainder Lower Tampa Bay", "Boca Ciega Bay South", "Terra Ceia Bay", "Manatee River"), 
  tnhyref = c(1.08, 1.62, 1.24, 0.97, 1.59, 0.97, 1.10, 1.80),
  tphyref = c(0.23, 1.28, 0.24, 0.14, 0.52, 0.06, 0.14, 0.37)
)

allsegdat <- read_sas('https://github.com/tbep-tech/load-estimates/raw/main/data/raw/tb_rasegsanntntph2o_8521.sas7bdat') %>% 
  mutate(
    bay_segment = case_when(
      BAY_SEG == 1 ~ 'Old Tampa Bay', 
      BAY_SEG == 2 ~ 'Hillsborough Bay', 
      BAY_SEG == 3 ~ 'Middle Tampa Bay', 
      BAY_SEG == 4 ~ 'Lower Tampa Bay', 
      BAY_SEG == 55 ~ 'Boca Ciega Bay South',
      BAY_SEG == 6 ~ 'Terra Ceia Bay', 
      BAY_SEG == 7 ~ 'Manatee River'
    )
  )

rltbdat <- read_sas('https://github.com/tbep-tech/load-estimates/raw/main/data/raw/tb_rasegsanntntph2o_8521.sas7bdat') %>% 
  mutate(
    bay_segment = case_when(
      BAY_SEG %in% c(55, 6, 7) ~ 'Remainder Lower Tampa Bay',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(bay_segment))

# combine
totab <- allsegdat %>% 
  bind_rows(rltbdat) %>% 
  rename(
    Year = YEAR, 
    tn_load = TN_tons, 
    tp_load = TP_tons, 
    hy_load = h2oload10e6m3
  ) %>% 
  group_by(Year, bay_segment) %>% 
  summarise(
    tn_load = sum(tn_load), 
    tp_load = sum(tp_load), 
    hy_load = sum(hy_load), 
    .groups = 'drop'
  ) %>% 
  left_join(reflns, by = 'bay_segment') %>% 
  mutate(
    tnhy = tn_load / hy_load, 
    tphy = tp_load / hy_load,
    bay_segment = factor(bay_segment, 
                         levels = c("Old Tampa Bay", "Hillsborough Bay", "Middle Tampa Bay", "Lower Tampa Bay", "Remainder Lower Tampa Bay", "Boca Ciega Bay South", "Terra Ceia Bay", "Manatee River")
    )
  ) %>% 
  select(Year, bay_segment, tnhy, tphy, tnhyref, tphyref) %>% 
  mutate(
    tnhy = round(tnhy, 2), 
    tphy = round(tphy, 2)
  ) %>% 
  unite('tnhy/tphy', tnhy, tphy, sep = ' / ') %>% 
  unite('tnhyref/tphyref', tnhyref, tphyref, sep = ' / ') %>% 
  mutate(`tnhyref/tphyref` = paste0('(', `tnhyref/tphyref`, ')')) %>% 
  unite('bay_segment', bay_segment, `tnhyref/tphyref`, sep = ' ') %>% 
  mutate(
    bay_segment = factor(bay_segment, levels = c("Old Tampa Bay (1.08 / 0.23)", 
                                                 "Hillsborough Bay (1.62 / 1.28)", "Middle Tampa Bay (1.24 / 0.24)", 
                                                 "Lower Tampa Bay (0.97 / 0.14)", "Remainder Lower Tampa Bay (1.59 / 0.52)", 
                                                 "Boca Ciega Bay South (0.97 / 0.06)", "Terra Ceia Bay (1.1 / 0.14)", "Manatee River (1.8 / 0.37)"))
  ) %>% 
  pivot_wider(names_from = 'bay_segment', values_from = 'tnhy/tphy', names_sort = T)

thmdqo <- function(x, fontsize, padding){
  flextable::colformat_double(x, na_str = '-', big.mark = '', digits = 0) %>% 
    flextable::colformat_char(na_str = '-') %>% 
    flextable::border_inner() %>% 
    flextable::align(align = 'center', part = 'all') %>% 
    flextable::align(align = 'left', j = 1, part = 'all') %>% 
    flextable::fontsize(size = fontsize, part = 'all') %>% 
    flextable::padding(padding = padding, part = 'all')
}

tab <- flextable(totab) %>% 
  thmdqo(8, 0) %>% 
  set_caption('Nitrogen (left) and phosphorus (right) delivery ratios [i.e. total nitrogen or total phosphorus load (tons/yr) per unit water (million m3) delivered] to each of the major bay segments. The 1992-1994 ratio targets are shown in parentheses in the column headers (left nitrogen, right phosphorus) for each bay segment and represent the arithmetic mean of those years. The 1992-1994 arithmetic mean ratio targets have been adopted by FDEP and accepted by EPA as the numeric nutrient criteria for the major bay  segments. These bay segments have been established as FDEP Estuary Nutrient Regions. The Remainder Lower Tampa Bay segment is inclusive of the Boca Ciega Bay South, Terra Ceia Bay, and Manatee River segments.')

save_as_docx(tab, path = here('docs/loadratiotab.docx'))
