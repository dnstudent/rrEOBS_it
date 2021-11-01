library(dplyr)
library(ggplot2)
library(osmdata)
library(readr)
library(terra)

region.cut <- function (place_name, format_out = "matrix", type = "lines", crs = "", ...) {
  region.bb <- getbb(place_name, format_out = format_out, ...)
  if (format_out == "matrix") {
    return(ext(c(t(region.bb))))
  }
  else if (format_out == "polygon") {
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

data.stations.eobs <- read_delim(file.stations.eobs,
                            delim = "|",
                            trim_ws = T,
                            col_names = T,
                            col_types = c("i", "c", "c", "d", "d", "d", "D", "D"),
                            lazy = F)
data.stations.eobs.it.tibble <- data.stations.eobs %>% dplyr::filter(COUNTRY == "ITALY")
# ggplot(data.stations.eobs.it, mapping = aes(x = LON, y = LAT)) + geom_point()
# 
# country.italy.bb <- getbb("Italy", featuretype = "Country")
# country.italy.extent <- ext(c(t(country.italy.bb)))
# state.emilia.bb <- getbb("Emilia-Romagna, Italy", featuretype = "State")
# state.emilia.borders <- getbb("Emilia-Romagna, Italy", featuretype = "State", format_out = "polygon")
# state.tuscany.borders <- getbb("Toscana, Italy", featuretype = "State", format_out = "polygon")
# state.emilia.extent <- ext(c(t(state.emilia.bb)))
# 
# eobs.elevs <- rast(file.eobs.elevs)
# plot(crop(eobs.elevs, state.emilia.extent), colNA = "blue")
# for (border in state.emilia.borders) {
#   plot(vect(border, type = "lines"), add = T)
# }
# plot(vect(state.tuscany.borders[[1]], type = "lines"), add = T)
