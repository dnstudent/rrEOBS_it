library(dplyr)
library(ggplot2)
library(osmdata)
library(readr)
library(terra)

source("api_key.R")

region.cut <- function(place_name, format_out = "matrix", type = "lines", crs = "", ...) {
  region.bb <- getbb(place_name, format_out = format_out, key = api_key, ...)
  if (format_out == "matrix") {
    return(ext(c(t(region.bb))))
  } else if (format_out == "polygon") {
    if (class(region.bb)[1] == "list") {
      return(map(region.bb, function(x) vect(x, type = type, crs = crs)))
    } else {
      return(list(vect(region.bb, type = type, crs = crs)))
    }
  }
  return(region.bb)
}

# File editato. Nell'originale ci sono alcune date che non tornano (e.g. 31 novembre)
file.stations.eobs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/stations_info_rr_v23.1e_corrected.txt"
file.eobs.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/elev_ens_0.1deg_reg_v23.1e.nc"

stations.eobs <- function(start = "1961-01-01", stop = "1990-12-31") {
  read_delim(file.stations.eobs,
    delim = "|",
    trim_ws = T,
    col_names = T,
    col_types = c("i", "c", "c", "d", "d", "d", "D", "D"),
    lazy = F
  ) %>%
    dplyr::filter(START <= as.Date(start) & as.Date(stop) <= STOP) %>%
    return()
}
stations.eobs.vect <- function(start = "1961-01-01", stop = "1990-12-31") {
  return(vect(stations.eobs(start, stop), geom = c("LON", "LAT")))
}

stations_op <- function(data, fun = length, start = "1961-01-01", stop = "1990-12-31", ...) {
  stations <- stations.eobs(start, stop) %>%
    dplyr::filter(...) %>%
    terra::vect(geom = c("LON", "LAT")) %>%
    terra::rasterize(data, fun = fun)
  stations[is.na(stations)] <- 0
  return(stations)
}
