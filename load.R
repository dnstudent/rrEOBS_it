library(purrr)
library(rgdal)
library(terra)

load.clino <- function(src.path,
                       nrows, ncols,
                       xmin, ymax, step,
                       layer.names = NULL,
                       src.crs = "+proj=longlat +datum=WGS84",
                       na.strings = "-9999") {
  if (length(src.path) == 1) src.path <- list(src.path)
  clino.rast <-
    purrr::map(src.path, function(src.fname) {
      scan(src.fname, na.strings = na.strings) %>%
        matrix(ncol = ncols, byrow = T) %>% 
        terra::rast()
    }) %>%
    terra::rast()
  crs(clino.rast) <- src.crs
  if (length(step) == 1) step <- c(step, step)
  ext(clino.rast) <- ext(xmin, xmin + step[1] * ncols, ymax - step[2] * nrows, ymax)
  if (!is.null(layer.names)) names(clino.rast) <- layer.names
  return(clino.rast)
}
