# Check MIROC output per cell (before averaging) and compare to GFDL
library(tidyverse)
library(tidync)
library(sf)
library(lubridate)
require(gridExtra)

# read in NMFS mask file
nmfs <- st_read('../data/Depth trimmed NMFS shapefiles/NMFS610-650.shp')
nmfs <- nmfs %>% st_transform(crs = 4326)

# make lat-lon box for the GOA
lonrange <- c(188, 235)
latrange <- c(52, 62)

################################################################################
# MIROC
# get list of files for this esm and run combination
miroc_files <- list.files(paste0('../data/MIROC/ssp585/'), full.names = T)
miroc_file <- miroc_files[1]

# load connection with miroc file
miroc_data <- tidync(miroc_file)

#make key MIROC with depths
###############################################################################################
# get time
gridtime <- miroc_data %>% 
  activate('D0') %>% # this is the time grid - different ESMs may have it different so check
  hyper_array()

t0 <- gridtime$time[1] # time variable name may change across ESMs

# get the grid for the area of interest. This contains the coordinates for the centroids of each grid cell
miroc_latlon <- miroc_data %>%
  hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
               lat = lat > latrange[1] & lat <= latrange[2],
               time = time == t0) %>%
  hyper_tibble() %>%
  select(lat, lon, lev) %>%
  # group_by(lon, lat) %>%
  # slice_max(lev) %>%
  # ungroup() %>%
  distinct() %>%
  mutate(idx = paste(lon, lat, sep = '_')) 

# ggplot(miroc_latlon, aes(lon, lat))+geom_raster()

# the mid-point of some of the ESM grid cells is on land, but the cell still exists because part of it overlaps with water
# Therefore, we need to construct a geometry around each centroid, and then intersect each cell to the NMFS areas mask

# write a function that, for each ESM cell, constructs the geometry of the cell around the centroid reported in the nc files
make_cell_geom <- function(coords) {
  lat = coords$lat
  lon = coords$lon
  this_cell <- st_polygon(
    list(matrix(c(
      lon-0.5, lat-0.5,
      lon+0.5, lat-0.5,
      lon+0.5, lat+0.5,
      lon-0.5, lat+0.5,
      lon-0.5, lat-0.5
    ), ncol = 2, byrow = T))
  )
  
  return(this_cell)
}

# construct cell geometry
miroc_sf <- miroc_latlon %>%
  group_by(lon, lat) %>%
  slice_max(lev) %>%
  ungroup() %>%
  nest(coords = c(lat, lon)) %>%
  mutate(geometry = purrr::map(coords, make_cell_geom)) %>%
  select(-coords) %>%
  st_as_sf(crs = 4326)

# view
miroc_sf %>% ggplot()+geom_sf(aes(fill = lev))

# intersect to the nmfs areas dataframe, get area of intersections, and store their index and NMFS area code
# this function takes a few minutes, so you want to run it only once per ESM
miroc_nmfs <- nmfs %>% 
  st_intersection(miroc_sf) %>%
  rowwise() %>%
  mutate(area = as.numeric(st_area(geometry)) / 1e6) %>%
  ungroup()

# view
miroc_nmfs %>% ggplot()+geom_sf(aes(fill = lev))+theme_bw()+labs(title = 'Intersection of MIROC 1x1 grid cells and NMFS areas')

# extract key
key_miroc <- miroc_nmfs %>%
  select(idx, NMFS_AREA, area, lev) %>%
  st_set_geometry(NULL) %>%
  distinct() %>%
  mutate(model = 'miroc')

#make GFDL key
###############################################################################################
# get time
# get list of files for this esm and run combination
gfdl_files <- list.files(paste0('../data/GFDL/ssp585/'), full.names = T)
gfdl_file <- gfdl_files[1]

# load connection with gfdl file
gfdl_data <- tidync(gfdl_file)
  
gridtime <- gfdl_data %>% 
  activate('D3') %>% # this is the time grid - different ESMs may have it different so check
  hyper_array()

t0 <- gridtime$time[1] # time variable name may change across ESMs

# get the grid for the area of interest. This contains the coordinates for the centroids of each grid cell
gfdl_latlon <- gfdl_data %>%
  hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
               lat = lat > latrange[1] & lat <= latrange[2],
               time = time == t0) %>%
  hyper_tibble() %>%
  select(lat, lon, lev) %>%
  # group_by(lon, lat) %>%
  # slice_max(lev) %>%
  # ungroup() %>%
  distinct() %>%
  mutate(idx = paste(lon, lat, sep = '_')) 

# ggplot(gfdl_latlon, aes(lon, lat))+geom_raster()

# the mid-point of some of the ESM grid cells is on land, but the cell still exists because part of it overlaps with water
# Therefore, we need to construct a geometry around each centroid, and then intersect each cell to the NMFS areas mask

# write a function that, for each ESM cell, constructs the geometry of the cell around the centroid reported in the nc files
make_cell_geom <- function(coords) {
  lat = coords$lat
  lon = coords$lon
  this_cell <- st_polygon(
    list(matrix(c(
      lon-0.5, lat-0.5,
      lon+0.5, lat-0.5,
      lon+0.5, lat+0.5,
      lon-0.5, lat+0.5,
      lon-0.5, lat-0.5
    ), ncol = 2, byrow = T))
  )
  
  return(this_cell)
}

# construct cell geometry
gfdl_sf <- gfdl_latlon %>%
  group_by(lon, lat) %>%
  slice_max(lev) %>%
  ungroup() %>%
  nest(coords = c(lat, lon)) %>%
  mutate(geometry = purrr::map(coords, make_cell_geom)) %>%
  select(-coords) %>%
  st_as_sf(crs = 4326)

# view
gfdl_sf %>% ggplot()+geom_sf(aes(fill = lev))

# intersect to the nmfs areas dataframe, get area of intersections, and store their index and NMFS area code
# this function takes a few minutes, so you want to run it only once per ESM
gfdl_nmfs <- nmfs %>% 
  st_intersection(gfdl_sf) %>%
  rowwise() %>%
  mutate(area = as.numeric(st_area(geometry)) / 1e6) %>%
  ungroup()

# view
gfdl_nmfs %>% ggplot()+geom_sf(aes(fill = lev))+theme_bw()+labs(title = 'Intersection of MIROC 1x1 grid cells and NMFS areas')

# extract key
key_gfdl <- gfdl_nmfs %>%
  select(idx, NMFS_AREA, area, lev) %>%
  st_set_geometry(NULL) %>%
  distinct() %>%
  mutate(model = 'gfdl')

#################################################################

# compare depth structure
# cells in common
miroc_cells <- miroc_latlon %>% pull(idx) %>% unique()
gfdl_cells <- gfdl_latlon %>% pull(idx) %>% unique()
all_cells <- intersect(miroc_cells, gfdl_cells)


for (i in 1:length(all_cells)){
  this_idx <- all_cells[i]
  
  datgfdl <- gfdl_latlon %>% 
    filter(idx == this_idx) %>%
    mutate(levtop = lag(lev, default = 0),
           levthick = lev-levtop,
           lyr = 1:nrow(.)) 
  
  datmiroc <- miroc_latlon %>% 
    filter(idx == this_idx) %>%
    mutate(levtop = lag(lev, default = 0),
           levthick = lev-levtop,
           lyr = 1:nrow(.)) 
  
  plot1 <- datgfdl %>%
    ggplot()+
    geom_bar(aes(x = idx, y = rev(levthick), fill = factor(lyr)), color = 'grey', stat = 'identity', position = 'stack')+
    scale_y_reverse()+
    labs(title = paste('N =', max(datgfdl$lyr), ', max depth =', max(datgfdl$lev)))
  
  plot2 <- datmiroc %>%
    ggplot()+
    geom_bar(aes(x = idx, y = rev(levthick), fill = factor(lyr)), color = 'grey', stat = 'identity', position = 'stack')+
    scale_y_reverse()+
    labs(title = paste('N =', max(datmiroc$lyr), ', max depth =', max(datmiroc$lev)))
  
  plot3 <- grid.arrange(plot1, plot2, ncol=2)
  ggsave(paste0('../output_debugging_layers/',this_idx,'.png'), plot3, width = 12, height = 8)
  
}



key_matched <- key_miroc %>%
  full_join(key_gfdl, by = c('idx','NMFS_AREA')) %>%
  select(NMFS_AREA, idx, lev.x, lev.y)

# mean depth by area for miroc
key_matched %>% drop_na() %>% group_by(NMFS_AREA) %>% summarise(miroc_lev = median(lev.x))
# mean depth by area for gfdl
key_matched %>% group_by(NMFS_AREA) %>% summarise(gfdl_lev = median(lev.y))

# depth of the bottom layer between MIROC and GFDL by NMFS area does not seem to be that different
# even taking medians changes things for areas 620 and 630 but not 610, 640, 650

