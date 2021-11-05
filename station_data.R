library(dplyr)
library(ggplot2)
library(osmdata)
library(readr)
library(terra)

region.cut <- function(place_name, format_out = "matrix", type = "lines", crs = "", ...) {
  region.bb <- getbb(place_name, format_out = format_out, ...)
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


file.stations.eobs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/stations_info_rr_v23.1e_corrected.txt"
file.eobs.elevs <- "/Users/davidenicoli/Local_Workspace/Datasets/EOBS/elev_ens_0.1deg_reg_v23.1e.nc"

stations.eobs <- read_delim(file.stations.eobs,
  delim = "|",
  trim_ws = T,
  col_names = T,
  col_types = c("i", "c", "c", "d", "d", "d", "D", "D"),
  lazy = F
)
stations.eobs.vect <- vect(stations.eobs, geom = c("LON", "LAT"))

station_density <- function(data, ...) {
  stations <- stations.eobs %>%
    dplyr::filter(...) %>%
    terra::vect(geom = c("LON", "LAT")) %>%
    terra::rasterize(data, fun = length)
  stations[is.na(stations)] <- 0
  return(stations)
}
