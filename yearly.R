library(colorspace)
library(dplyr)
library(purrr)
library(terra)

source("load.R")
source("station_data.R")
source("plotting.R")
source("utils.R")

################################
## LOADING high-res italian data
################################


# Upscaling
clino <-
  load.clino("yearly") %>%
  raster.aggregate(12, mean, na.rm = T)
names(clino) <- "1961-1991"
varnames(clino) <- "rr"
units(clino) <- "mm"

################################

################################
## LOADING E-OBS
################################
# eobs.file <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/rr_ens_mean_0.1deg_reg_v23.1e.nc"
# The real deal
# subset seleziona le date di interesse: 01-01-1961 -> 31-12-1990
# eobs.raster <- terra::rast(eobs.file)
# eobs.subset <- terra::subset(eobs.raster, 4019:14975)

# Original code: uncomment if you never run it before
# eobs.it <- crop(eobs.subset, clino.it.ycum.agg)
# names(eobs.it) <- time(eobs.it)
# varnames(eobs.it) <- varnames(eobs.raster)
# units(eobs.it) <- units(eobs.raster)
# writeRaster(eobs.it, paste0(path.temp, "temp_crop_year.tif"), overwrite = T)
# eobs.subset.resampled <- resample(eobs.subset, it.monthly.aggregated, method = "near", filename = paste0(path.temp, "temp_resamp_month.grd"), overwrite = T)

# Using cached raster:
# eobs <- terra::rast(paste0(path.yearly.temp, "temp_crop_year.tif"))
# units(eobs) <- "mm"
# time(eobs) <- as.Date(names(eobs))
# varnames(eobs) <- "rr"
# eobs.subset.resampled <- rast(paste0(path.temp, "temp_resamp_month.grd"))

# indici per raggruppare gli anni
# index <- as.numeric(format(time(eobs.it), format = "%Y"))
# eobs.it.ycum <- terra::mean(tapp(eobs.it, index, sum, na.rm = T), na.rm = T)
#
# dx <- xmin(clino.it.ycum.agg) - xmin(eobs.it.ycum)
# dy <- ymin(clino.it.ycum.agg) - ymin(eobs.it.ycum)

eobs <- rast(paste0(path.data, "eobs.yearly.cumulated_0.1deg.nc"))
correction_matrix <- clino - eobs

# Voglio un aggiustamento di calcoli su EOBS -> CLINO - EOBS
# Voglio che la mappa sia blu dove la matrice di correzione aggiunge pioggia -> blu valori positivi, marrone valori negativi
# pdf(paste0(path.results, "yearly.pdf"), height = 5, width = 5)
plot.regions("Italy", correction_matrix, 500, correction = T )
# dev.off()

# pdf(paste0(path.results, "comparison.pdf"), width = 10, height = 5, pointsize = 6)
# par(mfrow = c(1, 2))
# plot.regions(c("Valle d'Aosta, Italy", "Trentino, Italy"),
#   range = c(-300, 300),
#   geom = list(stations.eobs.it.vect),
#   featuretypes = "State",
#   borders = T,
#   colNA = "blue"
# )
# dev.off()



# correction.coarse.ext <- correction_matrix %>% (function(m) {
#   ext(
#     xmin(m), xmax(m) - crop_factor.x(5, m) * res(m)[1],
#     ymin(m), ymax(m)
#   )
# })
# 
# correction.coarse.mean <- terra::crop(correction_matrix, correction.coarse.ext) %>%
#   terra::aggregate(5, mean, na.rm = T)
# correction.coarse.sd <- terra::crop(correction_matrix, correction.coarse.ext) %>%
#   terra::aggregate(5, sd, na.rm = T)

correction.coarse.mean <- raster.aggregate(correction_matrix, 5, na.rm = T)

stations.it.density <- stations_op(correction.coarse.mean, length, na.fill = 0, COUNTRY == "ITALY")
stations.it.density.agg <- terra::classify(stations.it.density,
  c(0, 4, 10, terra::minmax(stations.it.density)[2]),
  # 27,
  # include.lowest = T
  othersNA = F
)

stations.it.density.agg[is.na(stations.it.density.agg)] <- 0
boxplot(correction.coarse.mean,
  stations.it.density.agg,
  main = "Correction value (mean) vs Station density",
  ylab = "Correction [mm]", xlab = "Station density"
)
# pdf(paste0(path.results, "corr_vs_dens.pdf"), pointsize = 12)
boxplot(correction.coarse.sd,
  stations.it.density.agg,
  main = "Correction value (std) vs Station density",
  ylab = "Correction [mm]", xlab = "Station density"
)
# dev.off()

plot(correction.coarse.sd)

aggregate.surroundings <- function(w, na.max = NULL, ...) {
  values <- w[-(length(w) %/% 2 + 1)]
  if ((!is.null(na.max)) && (sum(is.na(values)) > na.max)) {
    return(NA)
  }
  return(mean(values, ...))
}

for (region in c("Emilia-Romagna", "Toscana", "Calabria", "Trentino Alto Adige", "Sardegna", "Campania")) {
  borders <- region.cut(paste(region, ", Italy"), featuretype = "State")
  region.raster <- terra::crop(correction_matrix, borders)
  m <- terra::aggregate(region.raster, 5, mean, na.rm = T)
  s <- terra::aggregate(region.raster, 5, sd, na.rm = T)
  stations <- stations_op(m)
  par(mfrow = c(2, 2))
  boxplot(m, stations, main = paste(region, "mean"), ylab = "Correction [mm]", xlab = "Station density")
  boxplot(s, stations, main = paste(region, "std"), ylab = "Correction [mm]", xlab = "Station density")
  plot.regions(region, m, range=c(-300, 300), geoms = list(stations.eobs.it.vect), borders = F)
  region.surroundings <- focal(region.raster,
    w = 3,
    fun = function(w) aggregate.surroundings(w, na.max = 4, na.rm = T),
    na.rm = T
  )
  plot(region.raster, region.surroundings, main = paste(region, "correlation"))
  print(paste0(region, ": ", cor(values(region.surroundings), values(region.raster), use = "na.or.complete")[1]))
}
