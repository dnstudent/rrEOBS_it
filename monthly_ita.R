library(colorspace)
library(purrr)
library(terra)
library(rgdal)

source("load.R")

path.temp <- "/Users/davidenicoli/Local_Workspace/LabClima/eobs_clino_monthly/"
path.monthly <- "/Users/davidenicoli/Local_Workspace/Datasets/CLINO/CLIMATOLOGIE_PIOGGE_USATE_2020_09/"
path.results <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano/Uni/Workspace/LabClima/rrEOBS_it/results/"

################################
## LOADING high-res italian data
################################
### From provided file
files.monthly <- list.files(path.monthly, full.names=TRUE)
# Ordino i nomi dei file in modo che mi carichi i mesi in ordine
sorting_indices <- order(as.numeric(sub(".*_OK_|$", "", files.monthly)))
files.monthly <- files.monthly[sorting_indices]
nrows <- 1321
ncols <- 1476
xmin <-  6.50417
ymax <-  47.49583
step <-  1/120
# it.extent <- ext(xmin, xmin + ncols*resolution, ymax - nrows*resolution, ymax)
# it.monthly.layers <- map(files.monthly, function(fname) rast(matrix(scan(fname, na.strings = -9999), ncol = ncols, byrow = TRUE),
#                                                           crs = "+proj=longlat +datum=WGS84 +no_defs"))
# it.monthly <- rast(it.monthly.layers)
# Rinomino i layer con i nomi dei mesi
#names(it.monthly) <- month.name
# ext(it.monthly) <- it.extent
it.extent <- ext(xmin, xmin + step*ncols, ymax - step*(nrows-1), ymax)
clino.it.mmean.agg <-
  load.clino(files.monthly, nrows, ncols, xmin, ymax, step, layer.names = month.name) %>% 
  crop(it.extent) %>%
  aggregate(12, "mean", na.rm = T)

################
## LOADING E-OBS
################
eobs.file <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/rr_ens_mean_0.1deg_reg_v23.1e.nc"
# The real deal
# bands seleziona le date di interesse: 01-01-1961 -> 31-12-1990
# eobs.raster <- rast(eobs.file)
# eobs.subset <- subset(eobs.raster, 4019:14975)

# Original code: uncomment if you never run it before
# eobs.it <- crop(eobs.subset, clino.it.mmean.agg)
# names(eobs.it) <- time(eobs.it)
# varnames(eobs.it) <- varnames(eobs.raster)
# writeRaster(eobs.it, paste0(path.temp, "eobs_it_month.tif"), overwrite = T)
# eobs.subset.resampled <- resample(eobs.subset, it.monthly.aggregated, method = "near", filename = paste0(path.temp, "temp_resamp_month.grd"), overwrite = T)

# Using cached raster:
eobs.it <- rast(paste0(path.temp, "eobs_it_month.tif"))
time(eobs.it) <- as.Date(names(eobs.it))

# indici per raggruppare sui mesi
index <- as.numeric(format(time(eobs.it), format = "%Y%m"))
eobs.it.mcum <- tapp(eobs.it, index, "sum", filename = paste0(path.temp, "temp_cum_month_crop.tif"), overwrite = T)

# indici per raggruppare sugli anni
index <- as.numeric(substr(names(eobs.it.mcum), 6, 7))
eobs.it.mmean <- tapp(eobs.it.mcum, index, "mean")
# eobs.resampled.monthly.mean <- tapp(eobs.resampled.monthly.cumulated, index, "mean")
names(eobs.it.mmean) <- month.name
# names(eobs.resampled.monthly.mean) <- month.name

dx <- xmin(clino.it.mmean.agg) - xmin(eobs.it.mmean)
dy <- ymin(clino.it.mmean.agg) - ymin(eobs.it.mmean)

# Voglio un aggiustamento di calcoli su EOBS -> CLINO - EOBS
# Voglio che la mappa sia blu dove la matrice di correzione aggiunge pioggia -> blu valori positivi, marrone valori negativi
pdf(paste0(path.results, "monthly.pdf"), height = 15, width = 20)
(clino.it.mmean.agg - shift(eobs.it.mmean, dx, dy)) %>% 
  clamp(-150, 150) %>% 
  plot(col = rev(diverging_hcl(51, palette = "Vik")), range = c(-150, 150), colNA = "blue")
dev.off()
