library(colorspace)
library(terra)

source("geo_data.R")
source("station_data.R")


cmap.correction <- diverging_hcl(51, palette = "Blue-Red 3")

plot.correction <- function(x,
                            range = NULL,
                            col = cmap.correction,
                            colNA = "blue",
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


plot.raster <- function(x, correction = FALSE, ..., add = FALSE) {
  plot.r <- if (correction) {
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
    plot.raster(correction = F, add = add)
  return(x)
}

plot.regions2 <- function(regions,
                          x,
                          correction = FALSE,
                          range = NULL,
                          featuretypes = "State",
                          stations = FALSE,
                          borders = FALSE,
                          geoms = NULL,
                          ...) {
  if (length(featuretypes) == 1) {
    featuretypes <- rep_len(featuretypes, length(regions))
  }
  if (correction) {
    plot.r <- plot.correction
  } else {
    plot.r <- plot
  }

  for (i in 1:length(regions)) {
    region.extent <- region.bbox(regions[i],
      format_out = "matrix",
      featuretype = featuretypes[i]
    )
    x <- x %>% terra::crop(region.extent, snap = "out")
    if (!is.null(range)) {
      if (length(range) == 1) {
        range <- c(-range, range)
      }
      x %>%
        terra::clamp(lower = range[1], upper = range[2], values = T) %>%
        plot.r(range = range, ...)
    } else {
      plot.r(x, ...)
    }
    if (stations) {
      for (geom in geoms) {
        geom %>%
          terra::crop(region.extent) %>%
          plot(add = T)
      }
    }
    if (borders) {
      region.borders <- region.bbox(regions[i],
        featuretype = featuretypes[i],
        format_out = "polygon",
        crs = crs(x)
      )
      for (border in region.borders) {
        plot(border, add = T)
      }
    }
    if (!is.null(stations)) {
      stations.eobs.vect(stations[1], stations[2]) %>%
        terra::crop(region.extent) %>%
        plot()
    }
  }
}

plot.regions <- function(x,
                         regions,
                         featuretypes = "State",
                         correction = FALSE,
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
    r <- region.crop(x, regions[i], featuretypes[i]) %>%
      plot.raster(correction, ..., add = F)
    if(stations) plot.stations(r, start = start, stop = stop, add = TRUE)
    if(borders) plot.borders(r, regions[i], featuretypes[i], add = TRUE)
  }
}
