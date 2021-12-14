library(terra)

source("load.R")
source("utils.R")

###########################
# LOADING STUFF (from zero)
###########################

# arcis.full <- load.arcis()
# eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>% crop(arcis.full, snap="out")
# arcis <- raster.extend(arcis, eobs, filename = paste0(path.arcis.temp, "arcis_extended.tif"), overwrite = T)
# arcis.resampled <- raster.resample(arcis, eobs)


############################
# LOADING STUFF (from cache)
############################
arcis <- rast(paste0(path.arcis.temp, "arcis.resampled.nc"))
eobs <- rast(paste0(path.yearly.temp, "eobs.it.nc")) %>% crop(arcis)

index <- as.numeric(format(time(eobs), format = "%m"))
runs <- rle(index)$lengths

for (region in c("Trentino Alto Adige")) {
  arcis.means <- arcis %>%
    region.crop(region) %>% 
    raster.time.reduction(c("%Y", "%m"), "sum", na.rm = T) %>% # cumulation on days
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(30) %>% rep(runs) # disaggregate: replicate monthly means for each day
  arcis.anomalies <- arcis / arcis.means
  names(arcis.anomalies) <- time(arcis.anomalies)
  
  eobs.means <- eobs %>%
    region.crop(region) %>% 
    raster.time.reduction(c("%Y", "%m"), "sum", na.rm = T) %>% # cumulation on days
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(30) %>% rep(runs)
  eobs.anomalies <- eobs / eobs.means
}


