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

##
# ra 2224

# # frankenstein approach
# pth <- 'T:/03_BOARDS_COMMITTEES/05_TBNMC/TB_LOADS/2027_RA_Deliverables/2224/Total'

# # these are generally the same format
# ad2224 <- read.csv(file.path(pth, 'AD_2224AnnSegLoads.csv'), header = T)
# dps2224 <- read.csv(file.path(pth, 'DPS_2224AnnSeg.csv'), header = T)
# ips2224 <- read.csv(file.path(pth, 'IPS_ML_2224AnnSeg.csv'), header = T)
# spr2224 <- read.csv(file.path(pth, 'Spr_2224.csv'), header = T)

# tocmb <- list(AD = ad2224, DPS = dps2224, IPS = ips2224, SPR = spr2224) |> 
#   map(~ setNames(.x, tolower(names(.x))) |> 
#     mutate(across(any_of("h2oload"), ~ .x / 1e6)) |>
#     rename(h2oload10e6m3 = any_of("h2oload")) |>
#     select(bay_seg, year, tn_tons, tp_tons, h2oload10e6m3)
#   ) |> 
#   enframe(name = 'source') |> 
#   unnest('value')

# # gw requires calc for bcbs (subtract MR and TCB from RLTB)
# gw2224 <- read.csv(file.path(pth, 'GWLD2224.csv'), header = T) |> 
#   mutate(
#     tn_tons = tnld.kg.mnth. / 907.18474,
#     tp_tons = tpld.kg.mnth. / 907.18474,
#     h2oload10e6m3 = h2oload.m3.mnth. / 1e6
#   ) |> 
#   summarise(
#     tn_tons = sum(tn_tons),
#     tp_tons = sum(tp_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3), 
#     .by = c(year, segment)
#   ) |> 
#   rename(bay_seg = segment)

# gwrltb <- read.csv(file.path(pth, 'GWS_2224AnnRASeg.csv'), header = T) |> 
#   select(-tss_tons) |> 
#   filter(BAY_SEG %in% c(5567)) |> 
#   select(-source, -BAY_SEG)

# tosub <- gw2224 |> 
#   filter(bay_seg %in% c(6, 7)) |> 
#   summarise(
#     tn_tons = sum(tn_tons),
#     tp_tons = sum(tp_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3), 
#     .by = year
#   )
# bcbs <- full_join(gwrltb, tosub, by = 'year') |> 
#   mutate(
#     bay_seg = 55,
#     tn_tons = tn_tons.x - tn_tons.y,
#     tp_tons = tp_tons.x - tp_tons.y,
#     h2oload10e6m3 = h2oload10e6m3.x - h2oload10e6m3.y
#   ) |> 
#   select(bay_seg, year, tn_tons, tp_tons, h2oload10e6m3)

# gw2224 <- bind_rows(gw2224, bcbs) |> 
#   mutate(
#     source = 'GW'
#   )

# # nps has all but needs formatting
# nps2224 <- read.csv(file.path(gsub('Total$', 'NPS', pth), '19a_npsmod4_2224_26Sep25.csv'), header = T) |> 
#   mutate(
#     tn_tons = tnload / 907.18474,
#     tp_tons = tpload / 907.18474,
#     h2oload10e6m3 = h2oload / 1e6,
#     source = 'NPS'
#   ) |> 
#   summarise(
#     tn_tons = sum(tn_tons),
#     tp_tons = sum(tp_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3), 
#     .by = c(year, segment, source)
#   ) |> 
#   rename(bay_seg = segment)

# # combine all and create rltb
# ra2224 <- bind_rows(tocmb, gw2224, nps2224) 
# rltb <- ra2224 |> 
#   filter(bay_seg %in% c(55, 6, 7)) |> 
#   mutate(
#     bay_seg = 5567
#   ) |> 
#   summarise(
#     tn_tons = sum(tn_tons),
#     tp_tons = sum(tp_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3), 
#     .by = c(year, source, bay_seg)
#   )
# ra2224 <- ra2224 |> 
#   filter(!bay_seg %in% 5) |> 
#   bind_rows(rltb) |> 
#   rename(
#     BAY_SEG = bay_seg, 
#     YEAR = year, 
#     TN_tons = tn_tons,
#     TP_tons = tp_tons
#   ) |> 
#   mutate(
#     bay_segment = case_when(
#       BAY_SEG == 1 ~ 'Old Tampa Bay', 
#       BAY_SEG == 2 ~ 'Hillsborough Bay', 
#       BAY_SEG == 3 ~ 'Middle Tampa Bay', 
#       BAY_SEG == 4 ~ 'Lower Tampa Bay', 
#       BAY_SEG == 55 ~ 'Boca Ciega Bay South',
#       BAY_SEG == 6 ~ 'Terra Ceia Bay', 
#       BAY_SEG == 7 ~ 'Manatee River',
#       BAY_SEG == 5567 ~ 'Remainder Lower Tampa Bay'
#     )
#   )

# act <- read.csv(file.path(pth, 'TOTLoadsRASeg2224.csv'), header = T) |> 
#   select(
#     BAY_SEG,
#     YEAR,
#     TN_tons = TN_TONS,
#     TP_tons = TP_TONS, 
#     h2oload10e6m3
#   )
# est <- ra2224 |> 
#   summarise(
#     TN_tons = sum(TN_tons), 
#     TP_tons = sum(TP_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3),
#     .by = c(BAY_SEG, YEAR)
#   )

# rltbdat <- left_join(act, est, by = c('BAY_SEG', 'YEAR'), suffix = c('-act', '-est')) |> 
#   pivot_longer(contains(c("-act", "-est"))) |> 
#   separate(name, into = c('variable', 'type'), sep = '-') |>
#   pivot_wider(names_from = type, values_from = value)

# ggplot(rltbdat, aes(x = est, y = act, color = factor(BAY_SEG))) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = 'red') +
#   facet_wrap(~ variable, scales = 'free') +
#   labs(
#     title = '2224 RAST Seg Load Estimates vs Actual Loads',
#     x = 'Estimated Load',
#     y = 'Actual Load'
#   ) +
#   theme_minimal()

# ra2224sum <- ra2224 |> 
#   summarise(
#     TN_tons = sum(TN_tons), 
#     TP_tons = sum(TP_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3),
#     .by = c(BAY_SEG, YEAR, bay_segment)
#   )

# simplified approach using files from RP email on 10/30/25
pth <- 'T:/03_BOARDS_COMMITTEES/05_TBNMC/TB_LOADS/2027_RA_Deliverables/2224/Total'

ra2224hys <- read.csv(file.path(pth, 'HydroLoad2224SegAnn.csv'))

ra2224 <- read.csv(file.path(pth, 'Ann8SegLoadsbySource.csv')) |> 
  pivot_longer(names_to = 'source', values_to = 'load_tons', cols = -c(bay_segment, Year, variable)) |>
  pivot_wider(names_from = variable, values_from = load_tons) |> 
  summarise(
    TN_tons = sum(TN), 
    TP_tons = sum(TP),
    .by = c(Year, bay_segment)
  ) |> 
  full_join(ra2224hys, by = c('Year' = 'YEAR', 'bay_segment' = 'BAY_SEG')) |>
  rename(
    YEAR = Year,
    BAY_SEG = bay_segment,
    h2oload10e6m3 = Hydrologic.Load..Million.m3.yr.
  )

rltb <- ra2224 |> 
  filter(BAY_SEG %in% c(55, 6, 7)) |> 
  mutate(
    BAY_SEG = 5567
  ) |> 
  summarise(
    TN_tons = sum(TN_tons),
    TP_tons = sum(TP_tons),
    h2oload10e6m3 = sum(h2oload10e6m3), 
    .by = c(YEAR, BAY_SEG)
  )

ra2224 <- ra2224 |>
  filter(!BAY_SEG == 5) |> 
  bind_rows(rltb) |>
  mutate(
    bay_segment = case_when(
        BAY_SEG == 1 ~ 'Old Tampa Bay', 
        BAY_SEG == 2 ~ 'Hillsborough Bay', 
        BAY_SEG == 3 ~ 'Middle Tampa Bay', 
        BAY_SEG == 4 ~ 'Lower Tampa Bay', 
        BAY_SEG == 55 ~ 'Boca Ciega Bay South',
        BAY_SEG == 6 ~ 'Terra Ceia Bay', 
        BAY_SEG == 7 ~ 'Manatee River',
        BAY_SEG == 5567 ~ 'Remainder Lower Tampa Bay'
     )
  )

# act <- read.csv(file.path(pth, 'TOTLoadsRASeg2224.csv'), header = T) |> 
#   select(
#     BAY_SEG,
#     YEAR,
#     TN_tons = TN_TONS,
#     TP_tons = TP_TONS, 
#     h2oload10e6m3
#   )
# est <- ra2224 |> 
#   summarise(
#     TN_tons = sum(TN_tons), 
#     TP_tons = sum(TP_tons),
#     h2oload10e6m3 = sum(h2oload10e6m3),
#     .by = c(BAY_SEG, YEAR)
#   )

# rltbdat <- left_join(act, est, by = c('BAY_SEG', 'YEAR'), suffix = c('-act', '-est')) |> 
#   pivot_longer(contains(c("-act", "-est"))) |> 
#   separate(name, into = c('variable', 'type'), sep = '-') |>
#   pivot_wider(names_from = type, values_from = value)

# ggplot(rltbdat, aes(x = est, y = act, color = factor(BAY_SEG))) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = 'red') +
#   facet_wrap(~ variable, scales = 'free') +
#   labs(
#     title = '2224 RAST Seg Load Estimates vs Actual Loads',
#     x = 'Estimated Load',
#     y = 'Actual Load'
#   ) +
#   theme_minimal()

##
# combine
totab <- allsegdat %>% 
  bind_rows(rltbdat) %>% 
  bind_rows(ra2224) |> 
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
