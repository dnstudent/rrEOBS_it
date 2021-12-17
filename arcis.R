library(ggplot2)
library(terra)

source("geo_data.R")
source("load.R")
source("plotting.R")
source("utils.R")

from <- "1961-01-01"
to <- "2010-12-31"

###########################
# LOADING STUFF (from zero)
###########################
# arcis.full <- load.arcis(from, to)
# arcis.full <- rast(paste0(path.arcis.temp, "arcis.tif"), opts = c("NUM_THREADS=ALL_CPUS"))
# print("ArCIS Loaded")
# eobs <- load.eobs(from, to, extent = arcis.full, snap = "out")
# print("EOBS loaded")
# arcis <- raster.extend(arcis.full, eobs, snap = "out")
# print("ArCIS extended")
# writeRaster(arcis, paste0(path.arcis.temp, "arcis.extended.tif"), gdal = c("COMPRESS=NONE"))
# print("ArCIS saved")
# arcis.resampled <- raster.resample(arcis, eobs, na.rm = T)
# print("ArCIS resampled")


############################
# LOADING STUFF (from cache)
############################
arcis <- rast(paste0(path.data, "arcis.resampled.tif"))
eobs <- load.eobs(from, to, extent = arcis)
time(arcis) <- time(eobs)

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


# for (region in c("Trentino-Alto Adige", "Emilia-Romagna")) {
# Regional stuff
region <- "Trentino-Alto Adige"
region1 <- paste0(region, ", Italia")

arcis.regional <- region.crop(arcis.anomalies, region1, crop.out = T)
a <- terra::global(arcis.regional, "mean", na.rm = T)
a$time <- time(arcis.regional)
a$dataset <- "ArCIS"

eobs.regional <- region.crop(eobs.anomalies, region1, crop.out = T)
e <- terra::global(eobs.regional, "mean", na.rm = T)
e$time <- time(eobs.regional)
e$dataset <- "EOBS"


correlation <- cor(a$mean, e$mean)
print(paste0(region, ": ", correlation))

p <- ggplot(rbind(a[1:500,], e[1:500,]), mapping = aes(x = time, y = mean, colour = dataset)) +
  geom_line()
ggsave(paste0("corr_", region, ".pdf"), p)

# Histograms


# }


