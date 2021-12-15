library(terra)

source("geo_data.R")
source("load.R")
source("plotting.R")
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

# Expects a raster with time attribute, starting from 01-01 and ending with 12-31
# Not a generic function! Beware
anomalies <- function(x) {
  means <- x %>%
    raster.time.reduction(c("%Y", "%m"), "mean", na.rm = T) %>% # cumulation on days
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(length(runs) / 12) %>% rep(runs) # disaggregate: replicate monthly means for each day
  return(x / means)
}

arcis.anomalies <- anomalies(arcis)
eobs.anomalies <- anomalies(eobs)


for (region in c("Trentino-Alto Adige")) {
  arcis.regional <- region.crop(arcis.anomalies, paste0(region, ", Italia"))
  eobs.regional <- region.crop(eobs.anomalies, paste0(region, ", Italia"))
  plot.raster(arcis.regional, main = paste("Arcis", region), maxnl = 2)
  plot.raster(eobs.regional, main = paste("EOBS", region), maxnl = 2)
}


