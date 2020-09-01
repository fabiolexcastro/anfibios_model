


# Load libraries ---------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, gtools, RSAGA)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fls <- list.files('../data/raster/climate/worldclim/baseline/1km', full.names = T, pattern = '.tif$') %>% 
  mixedsort()
dem <- raster('../data/raster/dem/process/03arc_seconds/dem_up_1500.tif')

# SAGA
env <- rsaga.env(path = 'C:/Users/Usuario/Documents/saga-7.2.0_x64')

# Maximum temperature -----------------------------------------------------
tmax <- grep('tmax', fls, value = TRUE)
tmin <- grep('tmin', fls, value = TRUE)
temp <- c(tmax, tmin)

# GWR Function ------------------------------------------------------------
downscaling <- function(fle){
  
  print('Processing...!')
  rsl <- rsaga.geoprocessor(lib = 'statistics_regression',
                            module = 'GWR for Grid Downscaling',
                            param = list(PREDICTORS = '../data/raster/dem/process/03arc_seconds/dem_up_1500.tif',
                                         REGRESSION = paste0('../data/raster/climate/worldclim/baseline/1ha/', basename(fle)),
                                         DEPENDENT = fle),
                            env = env)
  
  print('Done!')
}


# Testing -----------------------------------------------------------------
downscaling(fle = tmax[1])

for(i in 1:12){
  downscaling(fle = tmax[i])
}


for(i in 1:24){
  downscaling(fle = temp)
}




