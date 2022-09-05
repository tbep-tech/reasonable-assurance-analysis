library(tidyverse)
library(sf)
library(here)
library(tibble)
library(ggmap)
library(ggspatial)
library(stringr)
library(grid)
library(USAboundaries)
library(gridExtra)

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

# additional targets for remainder here https://drive.google.com/file/d/1sH9Jx1PQTVrvTaJ_FxDtf3IC-hIVzdpV/view
trgs <- targets %>% 
  bind_rows(
    tibble(
      bay_segment = c('BCBS', 'TCB', 'MR'), 
      name = c('Boca Ciega Bay South', 'Terra Ceia Bay', 'Manatee River'),
      chla_thresh = c(6.3, 8.7, 8.8)
    )
  )

maxyr <- 2021
yrrng <- c(1975, maxyr)

p1 <- show_rathrplot(chldat, bay_segment = "OTB", thr = "chla", yrrng = yrrng, trgs = trgs, thrs = T)
p1leg <- g_legend(p1)
p1 <- p1 + theme(legend.position = 'none')
p2 <- show_rathrplot(chldat, bay_segment = "HB", thr = "chla", yrrng = yrrng, trgs = trgs, thrs = T) + theme(legend.position = 'none')
p3 <- show_rathrplot(chldat, bay_segment = "MTB", thr = "chla", yrrng = yrrng, trgs = trgs, thrs = T) + theme(legend.position = 'none')
p4 <- show_rathrplot(chldat, bay_segment = "LTB", thr = "chla", yrrng = yrrng, trgs = trgs, thrs = T) + theme(legend.position = 'none')
p5 <- show_rathrplot(chldat, bay_segment = "BCBS", thr = "chla", yrrng = yrrng, trgs = trgs, thrs = T) + theme(legend.position = 'none')

# align
# Get the widths
pA <- ggplot_gtable(ggplot_build(p1))
pB <- ggplot_gtable(ggplot_build(p2))
pC <- ggplot_gtable(ggplot_build(p3))
pD <- ggplot_gtable(ggplot_build(p4))
pE <- ggplot_gtable(ggplot_build(p5))
maxWidth = grid::unit.pmax(pA$widths[2:3], pB$widths[2:3], pD$widths[2:3], pD$widths[2:3], pE$widths[2:3])

# Set the widths
pA$widths[2:3] <- maxWidth
pB$widths[2:3] <- maxWidth
pC$widths[2:3] <- maxWidth
pD$widths[2:3] <- maxWidth
pE$widths[2:3] <- maxWidth

grid.arrange(
  p1leg,
  arrangeGrob(pA, pB, ncol = 2),
  arrangeGrob(pC, pD, ncol = 2),
  arrangeGrob(pE, ncol = 1),
  ncol = 1, heights = c(0.1, 1, 1, 1)
)