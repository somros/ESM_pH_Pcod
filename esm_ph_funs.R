# Functions to be used in the code that extracts pH from ESM data

#' Create a matching key for ESM cells and NMFS areas
#'
#' @param esm ESM product: one of GFDL, MIROC, CESM2
#' @param run ESM scenario, defaults to "historical" to make the key
#' @param spatial_mask sf object of the area of interest (NMFS areas in the simplest case)
#'
#' @description 
# 1. Extracts the ESM spatial grid from the first NetCDF file of the historical run
# 2. Constructs a spatial object around the centroid of each ESM cell and intersects the resulting grid to the NMFS areas
# 3. Calculates area of the overlap between each cell and the NMFS area mask, to weight values when averaging by area
#' @return Return a key ESM index to NMFS area
#' @export
#' 
#' 
make_esm_nmfs_key <- function(esm, run = 'historical', spatial_mask){
  # Build key to match ESM cells to NMFS areas
  # esm = 'MIROC'
  # spatial_mask <- nmfs
  
  # get list of files for this esm and run combination
  esm_files <- list.files(paste0('../data/', esm, '/', run, '/'), full.names = T)
  
  # get grid file for the ESM (first file of the series)
  esm_gridfile <- esm_files[1]
  
  # get its first time step
  # which grid do we need to use?
  if(esm == 'GFDL'){
    tg <- 'D3'
  } else if(esm == 'MIROC') {
    tg <- 'D0'
  }
  
  gridtime <- tidync(esm_gridfile) %>% 
    activate(tg) %>% # this is the time grid - different ESMs may have it different so check
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
  
  # view
  # esm_sf %>% ggplot()+geom_sf()
  
  # intersect to the nmfs areas dataframe, get area of intersections, and store their index and NMFS area code
  # this function takes a few minutes, so you want to run it only once per ESM
  esm_nmfs <- spatial_mask %>% 
    st_intersection(esm_sf) %>%
    rowwise() %>%
    mutate(area = as.numeric(st_area(geometry)) / 1e6) %>%
    ungroup()
  
  # view
  #esm_nmfs %>% ggplot()+geom_sf(aes(fill = factor(NMFS_AREA)))+theme_bw()+labs(title = 'Intersection of MIROC 1x1 grid cells and NMFS areas')

  
  # extract key
  key <- esm_nmfs %>%
    select(idx, NMFS_AREA, area) %>%
    st_set_geometry(NULL) %>%
    distinct()
  
  return(key)
}


#' Extract pH from ESM output and map it to NMFS areas
#'
#' @param esm ESM product: one of GFDL, MIROC, CESM2
#' @param run ESM scenario, defaults to "historical" to make the key
#' @param esm_slice one of "surface" or "bottom" for shallowest or deepest slice
#'
#' @description 
# 1. Loops across all files for each ESM-run combination
# 2. Pulls data from the ESM NetCDF files, takes slice of interest, subsets to cells of interest, assigns date
# 3. Computes average values of variable per month and NMFS area based on weights given by area overlap
#' @return Return a dataframe of mean values per time step and NMFS_area, binding results from all files
#' @export
#' 
#' 
get_esm_ph <- function(esm, run, esm_slice){
  
  # determine which key we need to use
  if(esm == 'GFDL'){
    key <- gfdl_key
  } else if (esm == 'MIROC'){
    key <- miroc_key
  } else {
    key <- cesm2_key
  }
  
  # get list of files for this esm and run combination
  esm_files <- list.files(paste0('../data/', esm, '/', run, '/'), full.names = T)
  
  # loop over esm files
  this_esm_scenario <- list()
  
  for(i in 1:length(esm_files)){
    # Read ESM data -----------------------------------------------------------
    
    esm_file <- esm_files[i]
    
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
    
    # get idx to subset ESM data
    these_cells <- key %>% pull(idx) %>% unique()
    
    # now pull data from netcdf
    this_esm_data <- esm_data %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                                               lat = lat > latrange[1] & lat <= latrange[2])
    
    # MIROC hist runs are all in one very large file, so let's index time beforehand if the model is MIROC
    if(esm == 'MIROC'){
      gridtime <- esm_data %>% 
        activate('D0') %>% # this is the time grid - different ESMs may have it different so check
        hyper_array()
      
      t <- gridtime$time # get time series of time steps
      tday <- origin + t # transform to dates based on origin
      tday1970 <- tday[tday > as.Date('1969-12-01')] # select dates after Dec 1969
      tidx <- pmatch(tday1970, tday) # find position in original time vector
      tsub <- t[tidx] # subset
      tstart <- tsub[1] # get first position
      
      this_esm_data <- esm_data %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                                                 lat = lat > latrange[1] & lat <= latrange[2],
                                                 time = time > tstart) %>%
        hyper_tibble() 
    } else {
      # for GFDL we selected nc files 1970 onwards so no need to filter time step
      this_esm_data <- esm_data %>% hyper_filter(lon = lon > lonrange[1] & lon <= lonrange[2], 
                                                 lat = lat > latrange[1] & lat <= latrange[2]) %>%
        hyper_tibble() 
    }
    
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
