library(dplyr)
library(readr)
library(terra)

source("geo_data.R")

# File editato. Nell'originale ci sono alcune date che non tornano (e.g. 31 novembre)
file.stations.eobs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/stations_info_rr_v23.1e_corrected.txt"
file.eobs.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/elev_ens_0.1deg_reg_v23.1e.nc"

stations <- readr::read_delim(file.stations.eobs,
  delim = "|",
  trim_ws = T,
  col_names = T,
  col_types = c("i", "c", "c", "d", "d", "d", "D", "D"),
  lazy = F
)

stations.eobs <- function(x, ..., start = "1961-01-01", stop = "1990-12-31", return.vect = TRUE) {
  s <- stations %>%
    dplyr::filter(
      START <= as.Date(start), as.Date(stop) <= STOP,
      xmin(x) <= LON, LON <= xmax(x),
      ymin(x) <= LAT, LAT <= ymax(x),
      ...
    )
  if (return.vect) {
    return(terra::vect(s, geom = c("LON", "LAT"), crs = crs(x)))
  }
  return(s)
}

stations_op <- function(x, ..., fun = length, na.fill = NULL, start = "1961-01-01", stop = "1990-12-31") {
  stations <- stations.eobs(x, ..., start = start, stop = stop, return.vect = T) %>%
    terra::rasterize(x, fun = fun)
  if (!is.null(na.fill)) {
    stations <- terra::subst(stations, NA, na.fill)
  }
  return(stations)
}
