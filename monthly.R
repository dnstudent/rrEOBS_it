library(colorspace)
library(purrr)
library(terra)
library(rgdal)

source("load.R")
source("plotting.R")
source("utils.R")

################################
## LOADING high-res italian data
################################
### From provided files
clino <-
  load.clino("monthly") %>%
  raster.aggregate(12, "mean", na.rm = T)

################
## LOADING E-OBS
################
# The real deal
eobs <- load.eobs(dates = 4019:14975, extent = clino)
ext(clino) <- ext(eobs)

eobs <- eobs %>%
  raster.time.reduction(c("%Y", "%m"), fun = "sum", na.rm = T) %>%
  raster.time.reduction(c("%m"), fun = "mean", na.rm = T)
names(eobs) <- month.name

correction_matrix <- clino - eobs

# Voglio un aggiustamento di calcoli su EOBS -> CLINO - EOBS
# Voglio che la mappa sia blu dove la matrice di correzione aggiunge pioggia -> blu valori positivi, marrone valori negativi
pdf(paste0(path.results, "monthly.pdf"),
    height = (3/4)*(ymax(correction_matrix) - ymin(correction_matrix)),
    width = 1*(xmax(correction_matrix) - xmin(correction_matrix) - 1.2),
    pointsize = 20)
plot.correction(correction_matrix, 50, mar = c(3.1, 3.1, 2.1, 3.5))
dev.off()
