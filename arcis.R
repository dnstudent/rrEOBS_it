packages <-  c("ggplot2", "tidyr", "zoo", "terra")
invisible(lapply(packages, library, character.only = T))

sources <- c("geo_data.R", "load.R", "plotting.R", "utils.R")
invisible(lapply(sources, source))

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
    raster.time.reduction(c("%Y", "%m"), "mean", na.rm = T) %>% # daily mean on month
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(length(runs) / 12) %>% rep(runs) # disaggregate: replicate monthly means for each day
  return(x / means)
}

arcis.anomalies <- anomalies(arcis)
eobs.anomalies <- anomalies(eobs)

group_rr <- function(anomalies, nclasses = 10) {
  terra::classify(anomalies,
                  c(-1, 0, 1/rev(1:(nclasses/2)), 2:(nclasses/2), Inf),
                  include.lowest = F, right = T) %>% values(mat = F) %>% return()
}

region <- "Trentino-Alto Adige"
# for (region in c("Trentino-Alto Adige", "Emilia-Romagna", "Veneto")) {
# Regional stuff

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
  # Sistemare le labels 
  nclasses <- 10 # must be even
  arcis.dist <- group_rr(arcis.regional, nclasses)
  pdf(paste0(path.results, "arcis_", region, ".pdf"))
  hist(arcis.dist[!is.na(arcis.dist)], main = "ArCIS", freq = F, breaks = seq(-0.5, 0.5 + nclasses, by = 1))
  dev.off()
  eobs.dist <- group_rr(eobs.regional, nclasses)
  pdf(paste0(path.results, "eobs_", region, ".pdf"))
  hist(eobs.dist[!is.na(eobs.dist)], main = "EOBS", freq = F, breaks = seq(-0.5, 0.5 + nclasses, by = 1))
  dev.off()
  
  # Trend
  win_size <- 1500
  pdf(paste0(path.results, "trend_", region, ".pdf"))
  plot(rollmean(a$mean - e$mean, win_size), type = "l")
  dev.off()
  
# }


