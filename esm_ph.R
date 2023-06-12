# Purpose
# This loads the GOA mask and maps the ROMS coordinates to the NMFS coordinates

rm(list = ls())

## Load files and dependencies
# install.packages("pacman")
pacman::p_load(tidyverse, tidync, sf, rnaturalearth, raster, data.table, maps, mapdata, viridis, lubridate)
select <- dplyr::select

# read in NMFS mask file
nmfs <- st_read('../data/Depth trimmed NMFS shapefiles/NMFS610-650.shp')
nmfs <- nmfs %>% st_transform(crs = 4326)

# make lat-lon box for the GOA
lonrange <- c(188, 235)
latrange <- c(52, 62)

# fetch functions
source('esm_ph_funs.R')

# make keys
gfdl_key <- make_esm_nmfs_key(esm = 'GFDL', spatial_mask = nmfs)
miroc_key <- make_esm_nmfs_key(esm = 'MIROC', spatial_mask = nmfs)
# cesm2_key <- make_esm_nmfs_key(esm = 'CESM2', spatial_mask = nmfs)

# create all combinations of esm and scenarios and slices
esm <- c('GFDL', 'MIROC')
run <- c('historical', 'ssp126', 'ssp585')
esm_slice <- c('surface', 'bottom')

opts <- expand.grid(esm, run, esm_slice) %>% set_names(c('esm','run','esm_slice'))

esm_ph <- purrr::pmap_dfr(opts, get_esm_ph)

# write out
esm_ph %>% write.csv('esm_ph_1000.csv', row.names = F)

# view
maxdate <- as.Date('2100-10-01')
esm_ph %>%
  mutate(date = as.Date(paste(year, month, '01', sep = '-'))) %>%
  filter(NMFS_AREA == 650) %>%
  ggplot(aes(x = date, y = ph_mean, color = run))+
  geom_line()+
  #geom_label_repel(aes(group = idx, label = ifelse(date == maxdate, lev, '')))+
  theme_bw()+
  labs(x = '', y = 'pH', title = 'NMFS area 650')+
  facet_grid(slice~esm, scales = 'free')
