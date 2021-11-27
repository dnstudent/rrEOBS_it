library(terra)
library(RColorBrewer)

source("load.R")
source("utils.R")

# file.eobs.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/elev_ens_0.1deg_reg_v23.1e.nc"
# file.it.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/GTOPO30/italian_elevs_GTOPO30.grd"

# eobs.elevs <- rast(file.eobs.elevs)
# it.elevs <- rast(file.it.elevs)

clino.elevs <- load.elevs.gtopo()
eobs.elevs <- load.elevs.eobs(extent = clino.elevs)

clino.elevs.aggregated <- raster.aggregate(clino.elevs, 12, na.rm = T)
ext(clino.elevs.aggregated) <- ext(eobs.elevs)

# it.extent = ext(xmin(clino.elevs), xmax(clino.elevs) - res(clino.elevs)[1], ymin(clino.elevs), ymax(clino.elevs) - res(clino.elevs)[2])

# clino.elevs.aggregated <- crop(aggregate(clino.elevs, 12, na.rm = T), it.extent)
# eobs.cropped.elevs <- crop(eobs.elevs, it.extent)
# eobs.resampled.elevs <- resample(eobs.elevs, it.aggregated.elevs, method = "near")

plot(clino.elevs.aggregated - eobs.elevs, col = brewer.pal(11, "RdBu"))
# plot(it.aggregated.elevs - eobs.resampled.elevs, col = brewer.pal(11, "RdBu"))

clino.elevs.resampled <- raster.resample(clino.elevs, eobs.elevs)
plot(clino.elevs.resampled - eobs.elevs)

