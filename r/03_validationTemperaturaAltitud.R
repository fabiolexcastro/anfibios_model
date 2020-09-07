
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, RSAGA, tidyverse, corrplot, Hmisc, gtools)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
dem <- raster('../data/raster/dem/process/30arc_seconds/dem_col.tif')
lim <- shapefile('../data/shapefiles/mask/zona_estudio30.shp')
prj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Cutting the DEM
dem <- raster::crop(dem, lim)
dem <- raster::mask(dem, lim)

# Climate data
clm <- list.files('../data/raster/climate/worldclim/baseline/1km', full.names = TRUE, pattern = '.tif$') %>% 
  mixedsort() %>% 
  stack()
clm <- projectRaster(clm, crs = prj)
clm_tbl <- rasterToPoints(clm) %>% as_tibble()

# Stacking DEM and Climate
dem <- raster::resample(dem, clm[[1]], method = 'ngb')

# Projecting the dem
dem_prj <- projectRaster(dem, crs = prj)
res <- res(dem_prj)[1] * res(dem_prj)[2]

par(mfrow = c(1, 2))
plot(dem_prj, main = 'DEM')
plot(clm[[1]], main = 'Precipitation')
dev.off(); par(mfrow = c(1, 1)) # Restart the frames for the plot

# Calculating the slope ---------------------------------------------------
slp <- terrain(x = dem_prj, opt = 'slope', unit = 'degrees') %>% projectRaster(., crs = crs(dem), method = 'bilinear')
asp <- terrain(x = dem_prj, opt = 'aspect', unit = 'degrees') %>% projectRaster(., crs = crs(dem), method = 'bilinear')
tri <- terrain(x = dem_prj, opt = 'TRI') %>% projectRaster(., crs = crs(dem), method = 'bilinear')
tpi <- terrain(x = dem_prj, opt = 'TPI') %>% projectRaster(., crs = crs(dem), method = 'bilinear')
trn <- list(slp, asp, tri, tpi)
trn <- raster::resample(trn, dem, method = 'bilinear')
Map('writeRaster', x = unstack(trn), filename = paste0('../data/raster/dem/process/30arc_seconds/', c('slope.tif', 'aspect.tif', 'tri.tif', 'tpi.tif')), overwrite = TRUE)
extent(trn)
extent(dem)
trn <- addLayer(dem, trn) 

# Extracting a sample -----------------------------------------------------
dfm <- rasterToPoints(stk, spatial = FALSE)
dfm <- as_tibble(dfm)



dem_tbl <- rasterToPoints(dem_prj)
dem_tbl <- as_tibble(dem_tbl)
n <- nrow(dem_tbl) * 0.05
cid <- cellFromXY(dem_prj, xy = as.data.frame(dem_tbl[,c(1,2)]))
dem_tbl <- dem_tbl %>% mutate(cellID = cid)
dem_tbl <- dem_tbl %>% dplyr::select(cellID, x, y, dem_up_1500)

dem_sub <- sample_n(tbl = dem_tbl, size = n, replace = FALSE)
saveRDS(object = dem_sub, file = '../rds/correlation/dem_sample_30.rds')


my_correlation <- function(var_x, var_y){
  
  print(paste0('To start ', var_x, ' + ', var_y))
  
  cor.test(var_x, var_y)
  cor(mtx$dem, mtx$tmax_1)
  rcorr(mtx[,c('dem', 'tmax_1')], type = 'spearson')
  
  print('Done')
  
}



mtx <- raster::extract(clm, dem_sub[,2:3])
mtx <- cbind(dem_sub, mtx)
mtx <- drop_na(as.data.frame(mtx))
mtx <- as_tibble(mtx)
mtx <- mtx %>% dplyr::select(cellID, x, y, dem = dem_up_1500, everything())

cor.test(mtx$dem, mtx$tmax_1)
cor(mtx$dem, mtx$tmax_1)
rcorr(mtx[,c('dem', 'tmax_1')], type = 'spearson')


