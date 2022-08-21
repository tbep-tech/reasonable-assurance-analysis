library(tidyverse)
library(sf)
library(here)
library(tibble)
library(ggmap)
library(ggspatial)

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

# plot
p <- ggmap(bsmap1_transparent) +
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

png(here('figs/lulcmap.png'), height = 5, width = 8, res = 300, units = 'in')
p
dev.off()
