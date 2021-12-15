library(osmdata)
library(purrr)
library(sf)
library(terra)

source("api_key.R")
source("load.R")

store.name <- function(region) {
  return(gsub("[^[:alpha:]]", "", region) %>% tolower())
}

store.filename <- function(region, store, ext) {
  return(paste0(path.borders, store, "/", store.name(region), ext))
}

store.exists <- function(region, store, ext) {
  return(file.exists(store.filename(region, store, ext)))
}

region.bbox <- function(region, featuretype = "State", ...) {
  if (!store.exists(region, "bbox", "") || length(list(...)) > 0) {
    region.bb <- getbb(region, featuretype = featuretype, format_out = "matrix", key = api_key, ...) %>%
      t() %>%
      c() %>%
      terra::ext()
    write.table(as.vector(region.bb), store.filename(region, "bbox", ""))
    return(region.bb)
  }
  return(store.filename(region, "bbox", "") %>%
           read.table() %>% t() %>% as.vector() %>% terra::ext())
}

region.borders <- function(region, featuretype = "State", ...) {
  if (!store.exists(region, "polygons", ".shp") || length(list(...)) > 0) {
    borders <- getbb(region, featuretype = featuretype, format_out = "sf_polygon", key = api_key, ...)
    if (class(borders)[1] == "list") borders <- borders$multipolygon
    sf::st_write(borders, store.filename(region, "polygons", ".shp"), quiet = T)
    return(borders)
  }
  return(sf::st_read(store.filename(region, "polygons", ".shp"), quiet = T))
}


region.crop <- function(x, region, featuretype = "State", crop.out = TRUE, ...) {
  if (!crop.out) {
    bb <- region.bbox(region, featuretype, ...)
    return(terra::crop(x, bb, snap = "out"))
  } else {
    borders <- vect(region.borders(region, featuretype, ...))
    crs(borders) <- crs(x)
    return(terra::mask(x %>% terra::crop(borders), borders) %>% terra::trim())
  }
}
