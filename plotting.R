library(colorspace)
library(terra)
source("station_data.R")


plot.correction <- function(correction, ...) {
  plot(correction, col = diverging_hcl(51, palette = "Vik"), colNA = "blue", ...)
}

plot.centered <- function(raster,
                          range,
                          correction = TRUE,
                          ...) {
  if (correction) {
    raster %>% terra::clamp(lower = range[1], upper = range[2]) %>% plot.correction(range = range, ...)
  } else {
    raster %>% terra::clamp(lower = range[1], upper = range[2]) %>% plot(range = range, ...)
  }
}


plot.regions <- function(regions,
                         raster,
                         range = NULL,
                         correction = FALSE,
                         featuretypes = "settlement",
                         geoms = NULL,
                         borders = FALSE,
                         ...) {
  if (length(featuretypes) == 1) {
    featuretypes <- rep_len(featuretypes, length(regions))
  }
  for (i in 1:length(regions)) {
    region.extent <- region.cut(regions[i],
                                format_out = "matrix",
                                featuretype = featuretypes[i]
    )
    raster <- raster %>% terra::crop(region.extent)
    if (!is.null(range)) {
      raster %>%
        terra::clamp(lower = range[1], upper = range[2], values = T) %>%
        plot(range = range, ...)
    } else {
      raster %>%
        plot(...)
    }
    if (!is.null(geoms)) {
      for (geom in geoms) {
        geom %>%
          terra::crop(region.extent) %>%
          plot(add = T)
      }
    }
    if (borders) {
      region.borders <- region.cut(regions[i],
                                   featuretype = featuretypes[i],
                                   format_out = "polygon",
                                   crs = crs(raster)
      )
      for (border in region.borders) {
        plot(border, add = T)
      }
    }
  }
}