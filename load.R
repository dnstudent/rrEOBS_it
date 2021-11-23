library(purrr)
library(rgdal)
library(terra)

path.yearly.temp <- "/Users/davidenicoli/Local_Workspace/LabClima/eobs_clino_yearly/"
path.monthly.temp <- "/Users/davidenicoli/Local_Workspace/LabClima/eobs_clino_monthly/"
path.yearly <- "/Users/davidenicoli/Local_Workspace/Datasets/CLINO/yearly/"
path.monthly <- "/Users/davidenicoli/Local_Workspace/Datasets/CLINO/CLIMATOLOGIE_PIOGGE_USATE_2020_09/"
path.results <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano/Uni/Workspace/LabClima/rrEOBS_it/results/"
path.data <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano/Uni/Workspace/LabClima/rrEOBS_it/rasters/"
path.eobs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/"
path.gtopo <- "/Users/davidenicoli/Local_Workspace/Datasets/GTOPO30/"

file.clino.yearly <- paste0(path.yearly, "CLINO_GRID_ITA_P_FINALE_MONTHLY_ASCII_ANNO")
file.clino.monthly <- list.files(path.monthly, full.names=TRUE)
# Ordino i nomi dei file in modo che mi carichi i mesi in ordine
sorting_indices <- order(as.numeric(sub(".*_OK_|$", "", file.clino.monthly)))
file.clino.monthly <- file.clino.monthly[sorting_indices]
file.eobs <- paste0(path.eobs, "rr_ens_mean_0.1deg_reg_v23.1e.nc")
file.eobs.elevs <- paste0(path.eobs, "elev_ens_0.1deg_reg_v23.1e.nc")
file.gtopo <- paste0(path.gtopo, "italian_elevs_GTOPO30.grd")

load.clino2 <- function(src.path, nrows, ncols,
                       xmin = 6.50417, ymax = 47.49583, step = 1 / 120,
                       layer.names = NULL,
                       src.crs = "+proj=longlat +datum=WGS84",
                       na.strings = "-9999") {
  if (length(src.path) == 1) src.path <- list(src.path)
  clino.rast <-
    purrr::map(src.path, function(src.fname) {
      scan(src.fname, na.strings = na.strings) %>%
        matrix(ncol = ncols, byrow = T) %>%
        terra::rast()
    }) %>%
    terra::rast()
  crs(clino.rast) <- src.crs
  if (length(step) == 1) step <- c(step, step)
  ext(clino.rast) <- ext(xmin, xmin + step[1] * ncols, ymax - step[2] * nrows, ymax)
  if (!is.null(layer.names)) names(clino.rast) <- layer.names
  return(clino.rast)
}

load.clino <- function(which) {
  switch(which,
         "yearly" = load.clino2(file.clino.yearly, 1320, 1476),
         "monthly" = load.clino2(file.clino.monthly, 1321, 1476, layer.names = month.name)
  ) %>% return()
}

load.eobs <- function(src.path = file.eobs, dates = 4019:14975, extent = NULL) {
  if(is.null(extent)) {
  return(src.path %>% terra::rast() %>% terra::subset(dates))
  } else {
    return(src.path %>% terra::rast() %>% terra::subset(dates) %>% terra::crop(extent))
  }
}

load.elevs.eobs <- function(src.path = file.eobs.elevs, extent = NULL) {
  if(is.null(extent)) {
    return(src.path %>% terra::rast())
  } else {
    return(src.path %>% terra::rast() %>% terra::crop(extent))
  }
}

load.elevs.gtopo <- function(src.path = file.gtopo, extent = NULL) {
  return(load.elevs.eobs(src.path, extent))
}
