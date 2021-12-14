library(colorspace)
library(terra)

source("geo_data.R")
source("station_data.R")


cmap.correction <- diverging_hcl(51, palette = "Blue-Red 3")

plot.correction <- function(x,
                            range = NULL,
                            col = cmap.correction,
                            colNA = "green",
                            ...) {
  if (is.null(range)) {
    mm <- minmax(x)
    val <- if (max(mm) > abs(min(mm))) max(mm) else abs(min(mm))
    range <- c(-val, val)
  } else if (length(range) == 1) {
    range <- c(-range, range)
  }
  x %>%
    terra::clamp(lower = range[1], upper = range[2]) %>%
    terra::plot(range = range, col = col, colNA = colNA, ...)
}


plot.raster <- function(x, is.correction = FALSE, ..., add = FALSE) {
  plot.r <- if (is.correction) {
    plot.correction
  } else {
    terra::plot
  }
  if (class(x) == "list") {
    plot.r(x[[1]], ..., add = add)
    for (r in x[-1]) {
      plot.r(r, ..., add = TRUE)
    }
  } else {
    plot.r(x, ..., add = add)
  }
  return(x)
}

plot.stations <- function(x, ..., start = "1961-01-01", stop = "1990-12-31", add = TRUE) {
  stations.eobs(x, ..., start = start, stop = stop, return.vect = TRUE) %>%
    terra::plot(add = add)
  return(x)
}

plot.borders <- function(x, region, featuretype = "State", add = TRUE) {
  borders <- region.borders(region, crs(x), featuretype = featuretype) %>%
    plot.raster(is.correction = F, add = add)
  return(x)
}

plot.region <- function(x,
                        region = NULL,
                        featuretype = "State",
                        is.correction = FALSE,
                        stations = FALSE,
                        start = "1961-01-01",
                        stop = "1990-12-31",
                        borders = FALSE,
                        station_args = c(), # Yet to be implemented
                        ...)
{
  if (!is.null(region)) {
    x <- region.crop(x, region, featuretype)
  }
  plot.raster(x, is.correction = is.correction, ..., add = F)
  if(stations) plot.stations(x, start = start, stop = stop, add = TRUE)
  if(borders) plot.borders(x, region, featuretype, add = TRUE)
  return(x)
}

plot.regions <- function(x,
                         regions,
                         featuretypes = "State",
                         is.correction = FALSE,
                         stations = FALSE,
                         start = "1961-01-01",
                         stop = "1990-12-31",
                         borders = FALSE,
                         station_args = c(), # Yet to be implemented
                         ...) {
  if (length(featuretypes) == 1) {
    featuretypes <- rep_len(featuretypes, length(regions))
  }
  for (i in 1:length(regions)) {
    plot.region(x, regions[i], featuretypes[i], is.correction, stations, start, stop, borders, station_args, ...)
  }
}
