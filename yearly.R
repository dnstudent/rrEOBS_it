library(colorspace)
library(dplyr)
library(purrr)
library(terra)

source("load.R")
source("station_data.R")
source("plotting.R")
source("utils.R")

from <- "1961-01-01"
to <- "1990-12-31"

################################
## LOADING high-res italian data
################################


# Upscaling
clino <-
  load.clino("yearly") %>%
  raster.aggregate(12, mean, na.rm = T)
names(clino) <- "1961-1990"
varnames(clino) <- "rr"
units(clino) <- "mm"

################################

################################
## LOADING E-OBS
################################
# eobs.file <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/rr_ens_mean_0.1deg_reg_v23.1e.nc"
# The real deal
# subset seleziona le date di interesse: 01-01-1961 -> 31-12-1990
eobs <- load.eobs(from, to, extent = clino)
ext(clino) <- ext(eobs)

# indici per raggruppare gli anni
index <- as.numeric(format(time(eobs), format = "%Y"))
eobs <- eobs %>% terra::tapp(index, sum, na.rm = T) %>% terra::mean(na.rm = T)
#
# dx <- xmin(clino.it.ycum.agg) - xmin(eobs.it.ycum)
# dy <- ymin(clino.it.ycum.agg) - ymin(eobs.it.ycum)

correction_matrix <- clino - eobs

# Voglio un aggiustamento di calcoli su EOBS -> CLINO - EOBS
# Voglio che la mappa sia blu dove la matrice di correzione aggiunge pioggia -> blu valori positivi, marrone valori negativi
pdf(paste0(path.results, "yearly.pdf"),
    height = ymax(correction_matrix) - ymin(correction_matrix),
    width = xmax(correction_matrix) - xmin(correction_matrix) -1.2,
    pointsize = 20)
plot.raster(correction_matrix, is.correction = T, range = 500)
dev.off()

for (region in c(
  "Veneto",
  "Trentino",
  "Emilia-Romagna",
  "Toscana"
)) {
  region1 <- paste0(region, ", Italy")
  pdf(paste0(path.results, region, "_y.pdf"), pointsize = 10, height = 5, width = 5)
  plot.region(correction_matrix,
              region1,
              featuretype = "State",
              is.correction = T,
              range = 300,
              stations = T,
              start = from,
              stop = to,
              borders = T,
              crop.out = F
  )
  dev.off()
}

correction.coarse.mean <- raster.aggregate(correction_matrix, 5, na.rm = T)
# 
# stations.density <- stations_op(correction.coarse.mean,
#                                 fun = function(x) x %>% length %>% terra::classify(c(0, 4, 10, Inf)),
#                                 na.fill = 0)
stations.density <- stations_op(correction.coarse.mean, from, to, fun = length, na.fill = 0)

stations.density.agg <- terra::classify(stations.density,
  c(0, 0, 1, 2, 4, 10, 15, 20, terra::minmax(stations.density)[2]),
  include.lowest = T,
  othersNA = F
)

pdf(paste0(path.results, "corr_vs_dens.pdf"),
    pointsize = 20)
boxplot(correction.coarse.mean,
  stations.density.agg,
  main = "Correzioni annuali",
  ylab = "Correzione [mm]", xlab = "Densità di stazioni"
)
dev.off()

elevs.complexity <- load.elevs.gtopo(extent = correction_matrix) %>%
  raster.aggregate(12, fun = sd, na.rm = T) %>%
  terra::classify(10)
