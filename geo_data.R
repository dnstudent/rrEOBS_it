library(osmdata)
library(purrr)
library(terra)

source("api_key.R")

region.bbox <- function(region, crs, ...) {
  region.bb <- getbb(region, format_out = "matrix", key = api_key, ...) %>%
    t %>% c %>% ext %>% 
    return()
}

region.borders <- function(region, crs, featuretype = "State", ...) {
  borders <- getbb(region, featuretype = featuretype, format_out = "polygon", key = api_key, ...)
  if (class(borders)[1] == "list") {
    borders <- purrr::map(borders, function(x) vect(x, type = "lines", crs = crs))
  } else {
    borders <- list(vect(borders, type = "lines", crs = crs))
  }
  return(borders)
}

region.crop <- function(x, region, featuretype = "State") {
  bb <- region.bbox(region, crs(x), featuretype = featuretype)
  return(terra::crop(x, bb, snap = "out"))
}