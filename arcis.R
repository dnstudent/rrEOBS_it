library(terra)

source("load.R")
source("utils.R")

arcis <- load.arcis()
new_res <- 0.1
new_ext <- ext(6.50417, 6.50417 + new_res*123, 47.49583-new_res*110, 47.49583)
eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>% crop(arcis, snap="out")
# arcis <- raster.extend(arcis, eobs, filename = paste0(path.arcis.temp, "arcis_extended.tif"))
arcis <- rast(paste0(path.arcis.temp, "arcis_extended.tif"))

arcis.resampled <- raster.resample(arcis, eobs)
