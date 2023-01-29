library(tidyverse)
library(sf)
library(here)
library(tibble)
library(ggmap)
library(ggspatial)
library(stringr)
library(grid)
library(USAboundaries)
library(patchwork)
library(tbeptools)
library(ggfx)
library(haven)
library(patchwork)
library(ggfx)

source(here('R/funcs.R'))

# current land use figure ---------------------------------------------------------------------

load(file = here('data/lulc2020.RData'))
load(file = here('data/sgdat2020.RData'))
load(file = here('data/fluccslkup.RData'))

# from T:\05_GIS\TBEP\2017_LULC_TBEP_Extent+2020_Seagrass.mxd
fluccsconv <- list(
  'Urban/Open Lands' = c(110, 120, 130, 140, 150, 170, 180, 182, 190, 740, 810, 820, 830),
  'Mining' = c(160),
  'Reclaimed Mined Lands' = c(165),
  'Agriculture' = c(210, 214, 220, 230, 240, 250, 255, 260),
  'Vegetated/Forested/Natural' = c(310, 320, 330, 410, 411, 412, 420, 434, 440, 710, 720),
  'Wetlands' = c(610, 611, 612, 615, 620, 621, 630, 641 ,642, 643, 644, 652, 653, 660),
  'Oyster' = c(654),
  'Tidal Flat' = c(651, 721),
  'Seagrass/SAV' = c(911, 912),
  'Water' = c(510, 520, 530, 540, 572)
) %>%
  enframe(value = 'LEV3') %>%
  unnest('LEV3') %>%
  left_join(fluccslkup, by = 'LEV3') %>%
  mutate(
    col = case_when(
      name == 'Urban/Open Lands' ~ '#FF0000',
      name == 'Mining' ~ '#C500FF',
      name == 'Reclaimed Mined Lands' ~ '#6D276B',
      name == 'Agriculture' ~ '#FFFF00',
      name == 'Vegetated/Forested/Natural' ~ '#00734C',
      name == 'Wetlands' ~ '#38A800',
      name == 'Oyster' ~ '#FFAA00',
      name == 'Tidal Flat' ~ '#FFEABE',
      name == 'Seagrass/SAV' ~ '#55FF00',
      name == 'Water' ~ '#0070FF'
    )
  )

# sums <- lulc2020 %>% 
#   left_join(fluccslkup, by = 'FLUCCSCODE') %>% 
#   mutate(
#     AREA_ACRES = st_area(.), 
#     AREA_ACRES = units::set_units(AREA_ACRES, 'acres'),
#     type = case_when(LEV3 %in% c(110,120,130,140,150,170,180,182,190,
#                                       740,810,820,830) ~ 'Developed',
#                           LEV3 %in% c(210,214,220,230,240,250,255,260,300) ~ 'Agriculture',
#                           LEV3 %in% c(310,320,330,410,411,412,420,430,434,440,
#                                       510,520,530,540,570,572,
#                                       610,611,612,615,620,621,630,640,641,642,643,644,646,
#                                       650,651,652,653,654,660,
#                                       710,720,721,
#                                       911,912) ~ 'Natural',
#                           LEV3 %in% c(160,165) ~ 'Mining')) %>% 
#   st_set_geometry(NULL) %>% 
#   group_by(type) %>% 
#   filter(!FLUCCSCODE %in% c(5400,6510,6540,7210,9113,9116,9121,9122)) %>% 
#   summarise(totals = sum(AREA_ACRES), .groups = 'drop') %>% 
#   mutate(pct = 100 * (totals / sum(totals)))

tomap1 <- lulc2020 %>%
  left_join(fluccsconv, by = 'FLUCCSCODE') %>%
  filter(!is.na(name) )%>%
  st_transform(crs = 4326)

tomap2 <- sgdat2020 %>%
  left_join(fluccsconv, by = 'FLUCCSCODE') %>%
  filter(!is.na(name)) %>%
  st_transform(crs = 4326)

# colors
cols <- fluccsconv %>%
  select(name, col) %>%
  unique %>%
  deframe()

# layer extent as bbox plus buffer
dat_ext <- tomap1 %>%
  st_bbox %>%
  st_as_sfc %>%
  st_buffer(dist = 0.04) %>%
  st_bbox %>%
  unname

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1,
                                         alpha.f = 0.2),
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# for inset
states <- us_states() %>%
  filter(name %in% c('Florida', 'Georgia', 'Alabama'))
ylimrng <- states %>%
  filter(name %in% 'Florida')
statebuff <- st_buffer(ylimrng, dist = 0.25)
insetbb <- dat_ext %>%
  st_as_sfc(crs = 4326)
insetylim <- st_bbox(statebuff)[c('ymin', 'ymax')]
insetxlim <- st_bbox(statebuff)[c('xmin', 'xmax')]

lbs1 <- tibble(
  lon = -85.9, lat = 25.6, label = 'Gulf of\nMexico'
) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
lbs2 <- tibble(
  lon = -81.2, lat = 29, label = 'Florida'
) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

pinset <- ggplot() +
  geom_sf(data = states, fill = 'grey', colour = 'grey') +
  geom_sf(data = insetbb, fill = NA, color = 'blue', size = 1.25) +
  geom_sf_text(data = lbs1, aes(label = label), size = 3.25) +
  geom_sf_text(data = lbs2, aes(label = label), size = 3.5, angle = -65) +
  coord_sf(ylim = insetylim, xlim = insetxlim) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'),
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  )

# plot
plulc <- ggmap(bsmap1_transparent) +
  geom_sf(data = tomap1, aes(fill = name), color = NA, inherit.aes = F, alpha = 1) +
  geom_sf(data = tomap2, aes(fill = name), color = NA, inherit.aes = F, alpha = 1) +
  scale_fill_manual(values = cols, drop = F) +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position  = 'right',
    legend.justification = 'top',
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA)
  ) +
  annotation_scale(location = 'tr') +
  annotation_north_arrow(location = 'bl', which_north = "true", height = grid::unit(0.75, "cm"),
                         width = grid::unit(0.75, "cm"))

p <- plulc +
  inset(ggplotGrob(pinset), xmin = -81.85, xmax = -81.35, ymin = 27.4, ymax = 27.7)

png(here('figs/lulcmap.png'), height = 5, width = 7, res = 300, units = 'in')
p
dev.off()

# wbid attainments ----------------------------------------------------------------------------

flimpaired <- st_read('T:/05_GIS/FDEP/Nutrients_305b_2021.shp') %>% 
  st_transform(crs = 4326)

attaincats <- list( 
  `2` = 'Attains some designated uses',
  `3a` = 'No data and information are available to determine if any designated use is attained',
  `3b` = 'Some data and information are available, but they are insufficient for determining if any designated use is attained',
  `3c` = 'Meets Planning List criteria and is potentially impaired for one or more designated uses',
  `4a` = 'Impaired for one or more designated uses and a TMDL has been completed',
  `4b` = 'Impaired for one or more designated uses, but no TMDL is required because an existing or proposed pollutant control mechanism provides reasonable assurance that the water will attain standards in the future',
  `4c` = 'Impaired for one or more designated uses but no TMDL is required because the impairment is not caused by a pollutant',
  `4d` = 'No causative pollutant has been identified',
  `4e` = 'Impaired, but recently completed or ongoing restoration activities should restore the designated uses of the waterbody',
  `5` = 'Water quality standards are not attained and a TMDL is required'
) %>% 
  enframe(name = 'category', value = 'description') %>%
  unnest('description') %>% 
  mutate(
    col = case_when(
      category == '2' ~ '#37A601',
      category == '3a' ~ '#CCCCCC',
      category == '3b' ~ '#828282',
      category == '3c' ~ '#FFFF01',
      category == '4a' ~ '#FF7E7E',
      category == '4b' ~ '#FFAA01',
      category == '4c' ~ '#FFCC99',
      category == '4d' ~ '#FFECB1', 
      category == '4e' ~ '#CCAB65',
      category == '5' ~ '#E80002'
    )
  ) %>% 
  unite('fulldescrip', category, description, sep = ': ', remove = F)  %>% 
  mutate(
    fulldescrip = str_wrap(fulldescrip, width = 80)
  )

tbimpaired <- flimpaired %>% 
  st_make_valid() %>% 
  st_intersection(tbshed) %>% 
  rename(category = Summary_As) %>% 
  select(WBID, category) %>% 
  inner_join(attaincats, by = 'category')

# colors 
cols <- attaincats %>% 
  select(fulldescrip, col) %>% 
  unique %>% 
  deframe() 

# layer extent as bbox plus buffer
dat_ext <- tbimpaired %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = 0.05) %>%
  st_bbox %>% 
  unname

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 12)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.2), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# plot
p <- ggmap(bsmap1_transparent) +
  geom_sf(data = tbimpaired, aes(fill = fulldescrip), color = 'black', inherit.aes = F, alpha = 0.8) +
  scale_fill_manual(values = cols, drop = F) +
  theme(
    legend.title = element_blank(), 
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    legend.position  = 'right', 
    legend.justification = 'top',
    axis.text.y = element_text(size = 7), 
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA)
  ) + 
  annotation_scale(location = 'tr') +
  annotation_north_arrow(location = 'bl', which_north = "true", height = grid::unit(0.75, "cm"), 
                         width = grid::unit(0.75, "cm"))

png(here('figs/wbidattainment.png'), height = 6, width = 9, res = 300, units = 'in')
p
dev.off()

# annual chl means all segments ---------------------------------------------------------------

load(file = here('data/chldat.RData'))

maxyr <- 2021
yrrng <- c(1975, maxyr)

p1 <- show_rathrplot(chldat, bay_segment = "OTB", thr = "chla", yrrng = yrrng, thrs = T)
p2 <- show_rathrplot(chldat, bay_segment = "HB", thr = "chla", yrrng = yrrng, thrs = T)
p3 <- show_rathrplot(chldat, bay_segment = "MTB", thr = "chla", yrrng = yrrng, thrs = T)
p4 <- show_rathrplot(chldat, bay_segment = "LTB", thr = "chla", yrrng = yrrng, thrs = T) 
p5 <- show_rathrplot(chldat, bay_segment = "BCBS", thr = "chla", yrrng = yrrng, thrs = T)
p6 <- show_rathrplot(chldat, bay_segment = "TCB", thr = "chla", yrrng = yrrng, thrs = T)
p7 <- show_rathrplot(chldat, bay_segment = "MR", thr = "chla", yrrng = yrrng, thrs = T) 

pout <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(ncol = 2, guides = 'collect') & 
  theme(
    legend.position = 'bottom'
  )

png(here('figs/chltrends.png'), height = 12, width = 12, res = 300, units = 'in')
pout
dev.off()

# tn lbs per year per person ------------------------------------------------------------------

# tn loads from load-estimates repo
load(url('https://github.com/tbep-tech/load-estimates/raw/main/data/totanndat.RData'))

# pop data from state-of-the-bay repo
load(url('https://github.com/tbep-tech/State-of-the-Bay/raw/master/data/popdat.RData'))

# T:\04_STAFF\ED\04_DATA\TB_LOADS\TNLoad_per_Capita_1976-2016_DRAFTLOADS_06022017.xlsx
# tn load from kg to tons
worstcase <- tibble(
  year = 1976, 
  tn_load = 9904, 
  pop = 1358245
)

loadpers <- totanndat %>% 
  filter(bay_segment %in% 'All Segments (- N. BCB)') %>% 
  select(year, tn_load) %>% 
  left_join(popdat, by = c('year' = 'yr')) %>% 
  bind_rows(worstcase) %>% 
  mutate(
    tn_load = tn_load * 2000, # tons to lbs
    yrcat = case_when(
      year <= 1976 ~ '1976\n(Worst Case)',
      year > 1976 & year <= 1990 ~ '1990\n(Loads from 1985-1990)',
      year > 1990 & year <= 2000 ~ '2000\n(Loads from 1991-2000)', 
      year > 2000 & year <= 2010 ~ '2010\n(Loads from 2001-2010)', 
      year > 2010 & year <= 2020 ~ '2020\n(Loads from 2011-2020)', 
      year > 2020 ~ '2021\n(Loads from 2021)'
    )
  ) %>% 
  group_by(yrcat) %>% 
  summarise(
    tn_load = mean(tn_load),
    pop = mean(pop), 
    .groups = 'drop'
  ) %>% 
  mutate(
    lbsyrper = tn_load / pop
  )

p <- ggplot(loadpers, aes(x = yrcat, y = lbsyrper)) + 
  with_shadow(geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 0.7, alpha = 0.5), sigma = 2.7, x_offset = 0, y_offset = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(loadpers$lbsyrper, na.rm = T))) + 
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = 'Time Period', 
    y = 'TN Load Entering Tampa Bay per Capita Estimate\n(lbs. / yr / person)'
  )

png(here('figs/tnpercapita.png'), height = 4, width = 9, res = 300, units = 'in')
print(p)
dev.off()

# tn v hy all segs ----------------------------------------------------------------------------

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
allsegdat <- allsegdat %>% 
  bind_rows(rltbdat) %>% 
  rename(
    year = YEAR, 
    tn_load = TN_tons, 
    tp_load = TP_tons, 
    hy_load = h2oload10e6m3
  ) %>% 
  group_by(year, bay_segment) %>% 
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
  )

p1 <- ggplot(allsegdat, aes(x = year, y = tnhy)) + 
  geom_line() + 
  geom_hline(aes(yintercept = tnhyref), linetype = 'dashed') + 
  geom_point() + 
  facet_wrap(~bay_segment, ncol = 1, scales = 'free_y') + 
  theme_minimal() + 
  labs(
    x = NULL, 
    y = 'tons / million m3', 
    title = '(a) Ratio of TN Load to Hydrologic Load'
  )

p2 <- ggplot(allsegdat, aes(x = year, y = tphy)) + 
  geom_line() + 
  geom_hline(aes(yintercept = tphyref), linetype = 'dashed') + 
  geom_point() + 
  facet_wrap(~bay_segment, ncol = 1, scales = 'free_y') + 
  theme_minimal() + 
  labs(
    x = NULL, 
    y = NULL, 
    title = '(b) Ratio of TP Load to Hydrologic Load'
  )

p <- p1 + p2 + plot_layout(ncol = 2, width = c(1, 0.95))

png(here('figs/loadratios.png'), height = 10, width = 9, res = 300, units = 'in')
print(p)
dev.off()

# seagrass coverage by bay segment ------------------------------------------------------------

fls <- list.files(path = '../hmpu-workflow/data', pattern = 'sgdat', full.names = T)

for(fl in fls){
  cat(fl, '\n')
  load(file = fl)
}

st_layers('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb')

sgdat2022 <- st_read('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb', 
                     layer = 'Seagrass_in_2022_Suncoast') %>% 
  select(FLUCCSCODE = FLUCCS_Code) %>% 
  filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
  st_cast('MULTIPOLYGON')



levs <- c('oldTampaBay', 'hillsboroughBay', 'middleTampaBay', 'lowerTampaBay', 'bocaCiegaBay', 'terraCieaBay', 'manateeRiver')
labs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Terra Ceia Bay', 'Manatee River')

segswfwmd <- st_read('T:/05_GIS/SWFWMD/Seagrass/2022_Seagrass/provisional/DraftMaps2022_1130.gdb/DraftMaps2022_1130.gdb', 
                     layer = 'suncoastSeagrassSegments') %>% 
  filter(waterbodyName %in% levs) %>% 
  mutate(
    waterbodyName = factor(waterbodyName, levels = levs, labels = labs)
  ) %>% 
  select(segment = waterbodyName)


sgyrs <- fls %>% 
  basename %>% 
  gsub('\\.RData$', '', .) %>% 
  c(., 'sgdat2022')

sgsegest <- NULL
for(sgyr in sgyrs){
  
  cat(sgyr, '\n')
  
  yr <- gsub('sgdat', '', sgyr)
  
  dat <- get(sgyr) %>% 
    filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
    st_union() %>% 
    st_transform(crs = st_crs(segswfwmd)) %>% 
    st_intersection(segswfwmd, .) %>% 
    mutate(
      acres = st_area(.), 
      acres = units::set_units(acres, 'acres'), 
      acres = as.numeric(acres)
    ) %>% 
    st_set_geometry(NULL) %>% 
    mutate(
      year = as.numeric(yr)
    )
  
  sgsegest <- rbind(sgsegest, dat)
  
}

toplo <- sgsegest %>% 
  mutate(acres = acres / 1000)

p <- ggplot(toplo, aes(x = factor(year), y = acres)) +
  geom_rect(xmin = 14.5, xmax = 17.5, ymin = -Inf, ymax = Inf, fill = 'darkgrey', alpha = 0.8) +
  with_shadow(geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 0.6), sigma = 2.7, x_offset = 0, y_offset = 0) +
  facet_wrap(~segment, ncol = 2, scales = 'free') +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        axis.text.y = element_text(colour = 'black'),
        plot.title = element_text(size = 22, colour = 'black'),
        legend.text = element_text(size = 16, colour = 'black'),
        axis.text.x = element_text(colour = 'black', angle = 45, size = 8, hjust = 1), 
        strip.background = element_blank(), 
        strip.text = element_text(hjust = 0, size = 13)
  ) + 
  labs(
    y = 'Seagrass Coverage (x1,000 acres)', 
    x = NULL
  )

png(here('figs/sgsegtrend.png'), height = 7, width = 7, res = 300, units = 'in')
p
dev.off()

