

# Load libraries ----------------------------------------------------------
install.packages('pacman')
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------
dem <- raster('../data/raster/dem/raw/tiles/30arc_seconds/gt30w100n40.tif')
col <- shapefile('../data/shapefiles/base/mgn_dpto_politico.shp')

col@data$gid <- 1

# Getting the Colombia boundaries -----------------------------------------
lim <- aggregate(col, 'gid')

# Extraction by mask ------------------------------------------------------
dem_lim <- raster::crop(dem, lim)
dem_lim <- raster::mask(dem_lim, lim)

dir.create('../data/raster/dem/process/30arc_seconds')
writeRaster(dem_lim, '../data/raster/dem/process/30arc_seconds/dem_col.tif')

# Filtering the DEM -------------------------------------------------------
positions <- which(dem_lim[] < 1500) 
dem_lim[positions] <- NA
plot(dem_lim)
writeRaster(dem_lim, '../data/raster/dem/process/30arc_seconds/dem_up_1500.tif')

# Making a nice map -------------------------------------------------------
dem_tbl <- rasterToPoints(dem_lim)
dem_tbl <- as_tibble(dem_tbl)

gg <- ggplot() +
  geom_tile(data = dem_tbl, aes(x = x, y = y, fill = gt30w100n40)) +
  geom_sf(data = st_as_sf(col), fill = NA) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = 'BrBG'), na.value = 'white') +
  labs(x = 'Longitud',
       y = 'Latitud',
       caption = 'Fuente: adaptado de NASA',
       fill = 'm.s.n.m') +
  theme(legend.position = 'bottom',
        legend.key.width = unit(5, 'line'))

dir.create('../png/maps', recursive = TRUE)
ggsave(plot = gg,
       filename = '../png/maps/dem_1500.png',
       units = 'in',
       width = 9, 
       height = 12, 
       dpi = 300)
