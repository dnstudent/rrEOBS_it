library(terra)

source("load.R")
source("utils.R")

###########################
# LOADING STUFF (from zero)
###########################

arcis.full <- load.arcis()
eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>% crop(arcis.full, snap="out")
# arcis <- raster.extend(arcis, eobs, filename = paste0(path.arcis.temp, "arcis_extended.tif"), overwrite = T)
# arcis.resampled <- raster.resample(arcis, eobs)


############################
# LOADING STUFF (from cache)
############################
arcis.resampled <- rast(paste0(path.arcis.temp, "arcis.resampled.nc"))
eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>% crop(arcis.resampled)
time(eobs) <- as.Date(names(eobs))

arcis.mean <- arcis.resampled %>%
  raster.time.reduction(c("%Y", "%m"), "sum", na.rm = T) %>% # cumulation on days
  raster.time.reduction(c("%m"), "mean", na.rm = T) # monthly mean


eobs.mean <- eobs %>%
  raster.time.reduction(c("%Y", "%m"), "sum", na.rm = T) %>% # cumulation on days
  raster.time.reduction(c("%m"), "mean", na.rm = T) # monthly mean


index <- as.numeric(format(time(arcis.resampled), format = "%m"))


