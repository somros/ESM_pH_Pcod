# Purpose
# This loads the GOA mask and maps the ROMS coordinates to the NMFS coordinates

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

esm <- 'GFDL'
run <- 'historical'
esm_slice <- c('surface', 'bottom')

opts <- expand.grid(esm, run, esm_slice) %>% set_names(c('esm','run','esm_slice'))

get_esm_ph <- function(esm, run, esm_slice){
  
  # Build key to match ESM cells to NMFS areas ------------------------------
  
  # get list of files for this esm and run combination
  esm_files <- list.files(paste0('../data/', esm, '/', run, '/'), full.names = T)
  
  # get grid file for the ESM (first file of the series)
  esm_gridfile <- esm_files[1]
  
  # get its first time step
  gridtime <- tidync(esm_gridfile) %>% 
    activate("D3") %>% # this is the time grid - different ESMs may have it different so check
    hyper_array()
  
  t0 <- gridtime$time[1] # time variable name may change across ESMs
  
  # get the grid for the area of interest. This contains the coordinates for the centroids of each grid cell
  esm_latlon <- tidync(esm_gridfile) %>%
    hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                 lat = lat > latrange[1] & lat <= latrange[2],
                 time = time == t0) %>%
    hyper_tibble() %>%
    select(lat, lon) %>%
    distinct() %>%
    mutate(idx = paste(lon, lat, sep = '_')) 
  
  # ggplot(esm_latlon, aes(lon, lat))+geom_raster()
  
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
  esm_sf <- esm_latlon %>%
    nest(coords = c(lat, lon)) %>%
    mutate(geometry = purrr::map(coords, make_cell_geom)) %>%
    select(-coords) %>%
    st_as_sf(crs = 4326)
  
  # intersect to the nmfs areas dataframe, get area of intersections, and store their index and NMFS area code
  # this function takes a few minutes, so you want to run it only once per ESM
  esm_nmfs <- nmfs %>% 
    st_intersection(esm_sf) %>%
    rowwise() %>%
    mutate(area = as.numeric(st_area(geometry)) / 1e6) %>%
    ungroup()
  
  # view
  # esm_nmfs %>% ggplot()+geom_sf(aes(fill = factor(NMFS_AREA)))
  
  # extract key
  key <- esm_nmfs %>%
    select(idx, NMFS_AREA, area) %>%
    st_set_geometry(NULL) %>%
    distinct()
  
  # loop over esm files
  this_esm_scenario <- list()
  
  for(i in 1:length(esm_files)){
    # Read ESM data -----------------------------------------------------------
    
    # load connection with ESM file
    esm_data <- tidync(esm_file)
    
    # extract origin (time) - to do for each ESM file
    origin <- ncmeta::nc_atts(esm_file, "time") %>% 
      tidyr::unnest(cols = c(value)) %>%
      filter(name == 'units') %>%
      select(value) %>%
      mutate(value = str_replace(value, 'days since ', '')) %>%
      pull(value) %>%
      as.Date()
    
    # ggplot(data=esm_nmfs)+
    #   geom_sf(aes(fill = factor(NMFS_AREA)))
    
    # get idx to subset ESM data
    these_cells <- esm_nmfs %>% pull(idx) %>% unique()
    
    # now pull data from netcdf
    this_esm_data <- esm_data %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                                               lat = lat > latrange[1] & lat <= latrange[2]) %>%
      hyper_tibble() 
    
    # get surface or bottom slice from ESM, assuming each to be representative of surface and bottom respectively
    if(esm_slice == 'surface') {
      this_esm_data <- this_esm_data %>%
        group_by(time, lon, lat) %>%
        slice_min(lev) %>%
        ungroup()
    } else {
      this_esm_data <- this_esm_data %>%
        group_by(time, lon, lat) %>%
        slice_max(lev) %>%
        ungroup()
    }
    
    # add time and cell index
    this_esm_data <- this_esm_data %>% 
      mutate(date = origin + time, # add time
             year = year(date),
             month = month(date),
             idx = paste(lon, lat, sep = '_')) %>% # add idx for subsetting based on key
      filter(idx %in% these_cells)
    
    # join in the NMFS area
    this_esm_data <- this_esm_data %>%
      left_join(key, by = 'idx')
    
    # now group by time step and NMFS area, and average bottom ph weighting by cell area
    mean_ph <- this_esm_data %>%
      group_by(year, month, NMFS_AREA) %>%
      summarise(ph_mean = weighted.mean(ph, area)) %>%
      ungroup() %>%
      mutate(esm = esm,
             run = run,
             slice = esm_slice)
    
    this_esm_scenario[[i]] <- mean_ph
  }
  
  this_esm_scenario_df <- rbindlist(this_esm_scenario)
  
  return(this_esm_scenario_df)
  
}

esm_ph <- purrr::pmap_dfr(opts, get_esm_ph)

# view
esm_ph %>%
  ggplot(aes(x = as.Date(paste(year, month, '01', sep = '-')), y = ph_mean, color = slice))+
  geom_line()+
  theme_bw()+
  labs(x = '', y = 'pH')+
  facet_wrap(~NMFS_AREA, scales = 'free')
