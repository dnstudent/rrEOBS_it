library(terra)


trim_size.col <- function(x, fact) {
  return(ncol(x) %% fact)
}
trim_size.row <- function(x, fact) {
  return(nrow(x) %% fact)
}

raster.trim <- function(x, nrow, ncol, where = NULL) {
  sx <- res(x)[1]
  sy <- res(x)[2]
  new.ext <- terra::ext(
    xmin(x) + (ncol %/% 2) * sx, xmax(x) - (ncol - (ncol %/% 2)) * sx,
    ymin(x) + (nrow %/% 2) * sy, ymax(x) - (nrow - (nrow %/% 2)) * sy
  )
  return(terra::crop(x, new.ext))
}

raster.aggregate <- function(x, fact, ..., crop = TRUE) {
  if (crop) x <- raster.trim(x, trim_size.row(x, fact), trim_size.col(x, fact))
  return(terra::aggregate(x, fact, ...))
}

aggregate.surroundings <- function(w, fun = mean, na.max = NULL, ...) {
  values <- w[-(length(w) %/% 2 + 1)]
  if ((!is.null(na.max)) && (sum(is.na(values)) > na.max)) {
    return(NA)
  }
  return(fun(values, ...))
}

raster.aggregate.surroundings <- function(x, fun = mean, na.max = NULL, ...) {
  return(terra::focal(x, w = 3, fun = function(w) aggregate.surroundings(w, fun, na.max, na.rm = T, ...), na.rm = T))
}
