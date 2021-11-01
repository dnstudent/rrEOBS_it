library(colorspace)
library(purrr)
library(terra)

path.temp <- "/Users/davidenicoli/Local_Workspace/LabClima/eobs_clino_yearly/"
path.yearly <- "/Users/davidenicoli/Local_Workspace/Datasets/CLINO/yearly/"
path.results <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano/Uni/Workspace/LabClima/rrEOBS_it/"

################################
## LOADING high-res italian data
################################

### From provided file
# file.clino <- paste0(path.yearly, "CLINO_GRID_ITA_P_FINALE_MONTHLY_ASCII_ANNO")
# nrows <- 1320
# ncols <-  1476
# xmin <-  6.50417
# ymax <-  47.49583
# resolution = 1/120
# clino.it <- rast(matrix(scan(file.clino), ncol = ncols, byrow = TRUE),
#                  crs = "+proj=longlat +datum=WGS84")
# ext(clino.it) <- ext(xmin, xmin + resolution*ncols, ymax - resolution*nrows, ymax)
# varnames(clino.it) <- "rr"
# names(clino.it) <- "yearly_mean"
# writeRaster(clino.it, paste0(path.yearly, "clino_rr.tif"), overwrite = T)

### From cache
clino.it.ycum <- rast(paste0(path.yearly, "clino_rr.tif"))
# Upscaling
clino.it.ycum.agg <- aggregate(clino.it.ycum, 12, "mean", na.rm = T)

################################

################################
## LOADING E-OBS
################################
eobs.file <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/rr_ens_mean_0.1deg_reg_v23.1e.nc"
# The real deal
# subset seleziona le date di interesse: 01-01-1961 -> 31-12-1990
eobs.raster <- rast(eobs.file)
eobs.subset <- subset(eobs.raster, 4019:14975)

# Original code: uncomment if you never run it before
# eobs.it <- crop(eobs.subset, clino.it.ycum.agg)
# names(eobs.it) <- time(eobs.it)
# varnames(eobs.it) <- varnames(eobs.raster)
# units(eobs.it) <- units(eobs.raster)
# writeRaster(eobs.it, paste0(path.temp, "temp_crop_year.tif"), overwrite = T)
# eobs.subset.resampled <- resample(eobs.subset, it.monthly.aggregated, method = "near", filename = paste0(path.temp, "temp_resamp_month.grd"), overwrite = T)

# Using cached raster:
eobs.it <- rast(paste0(path.temp, "temp_crop_year.tif"))
units(eobs.it) <- "mm"
time(eobs.it) <- as.Date(names(eobs.it))
# eobs.subset.resampled <- rast(paste0(path.temp, "temp_resamp_month.grd"))

# indici per raggruppare gli anni
index <- as.numeric(format(time(eobs.it), format = "%Y"))
eobs.it.ycum <- mean(tapp(eobs.it, index, "sum"))

dx <- xmin(clino.it.ycum.agg) - xmin(eobs.it.ycum)
dy <- ymin(clino.it.ycum.agg) - ymin(eobs.it.ycum)

# Voglio un aggiustamento di calcoli su EOBS -> CLINO - EOBS
# Voglio che la mappa sia blu dove la matrice di correzione aggiunge pioggia -> blu valori positivi, marrone valori negativi
pdf(paste0(path.results, "yearly.pdf"), height = 5, width = 5)
plot(clino.it.ycum.agg - shift(eobs.it.ycum, dx, dy), col = rev(diverging_hcl(51, palette = "Vik")), range = c(-1000, 1000))
dev.off()

source("station_data.R")

plot_comparison <- function(regions,
                            range,
                            data = clino.it.ycum.agg,
                            model = eobs.it.ycum,
                            featuretypes = "settlement",
                            col = rev(diverging_hcl(51, palette = "Vik")),
                            geoms = NULL,
                            borders = F,
                            ...) {
  if (length(featuretypes) == 1) {
    featuretypes <-  rep_len(featuretypes, length(regions))
  }
  for (i in 1:length(regions)) {
    region.extent <- region.cut(regions[i],
                                format_out = "matrix",
                                featuretype = featuretypes[i])
    dx <- xmin(data) - xmin(model)
    dy <- ymin(data) - ymin(model)
    crop(data - shift(model, dx, dy), region.extent) %>% 
      clamp(lower = range[1], upper = range[2], values = T) %>% 
      plot(col = col, range = range, ...)
    if (!is.null(geoms)) {
      for (geom in geoms) {
        crop(geom, region.extent) %>%
          plot(add = T)
      }
    }
    if (borders) {
      region.borders <- region.cut(regions[i],
                                   featuretype = featuretypes[i],
                                   format_out = "polygon",
                                   crs = crs(data))
      for (border in region.borders) {
        plot(border, add = T)
      }
    }
  }
}

data.stations.eobs.it <- vect(data.stations.eobs.it.tibble, geom = c("LON", "LAT"))
pdf(paste0(path.results, "comparison.pdf"), width = 10, height = 5, pointsize = 6)
par(mfrow = c(1, 2))
tryCatch(plot_comparison(c("Emilia-Romagna, Italy", "Trentino Alto Adige, Italy"),
                range = c(-300, 300),
                geom = list(data.stations.eobs.it),
                featuretypes = "State",
                borders = T,
                colNA = "blue"),
         error = function () {dev.off()})
dev.off()