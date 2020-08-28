
# Load libraries --------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
dem1 <- raster('../data/raster/dem/raw/tiles/03arc_seconds/cut_n00w090.tif')
dem2 <- raster('../data/raster/dem/raw/tiles/03arc_seconds/cut_s30w090.tif')
limt <- shapefile('../data/shapefiles/base/mgn_dpto_politico.shp')

# Mosaicking --------------------------------------------------------------
dem3 <- raster::mosaic(dem1, dem2, fun = 'mean')
dem3
dem_col <- raster::crop(dem3, limt)
dem_col <- raster::mask(dem_col, limt)
plot(dem_col)

dem_co2 <- dem_col
positions <- which(dem_co2[] < 1500) 
dem_co2[positions] <- NA

plot(dem_co2)
writeRaster(dem_co2, '../data/raster/dem/process/03arc_seconds/dem_up_1500.tif')


# Raster to polygon --------------------------------------------------------
dem_co3 <- dem_co2 * 0
dem_mask <- rasterToPolygons(dem_co3, dissolve = TRUE)
  
