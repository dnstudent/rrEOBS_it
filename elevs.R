library(terra)
library(RColorBrewer)

file.eobs.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/elev_ens_0.1deg_reg_v23.1e.nc"
file.it.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/GTOPO30/italian_elevs_GTOPO30.grd"

eobs.elevs <- rast(file.eobs.elevs)
it.elevs <- rast(file.it.elevs)
it.extent = ext(xmin(it.elevs), xmax(it.elevs) - res(it.elevs)[1], ymin(it.elevs), ymax(it.elevs) - res(it.elevs)[2])

it.aggregated.elevs <- crop(aggregate(it.elevs, 12, na.rm = T), it.extent)
eobs.cropped.elevs <- crop(eobs.elevs, it.extent)
eobs.resampled.elevs <- resample(eobs.elevs, it.aggregated.elevs, method = "near")

plot(it.aggregated.elevs - eobs.cropped.elevs, col = brewer.pal(11, "RdBu"))
plot(it.aggregated.elevs - eobs.resampled.elevs, col = brewer.pal(11, "RdBu"))

