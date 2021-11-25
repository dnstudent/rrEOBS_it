library(progress)
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

cell.aggregate.surroundings <- function(w, fun = mean, na.max = NULL, ...) {
  values <- w[-(length(w) %/% 2 + 1)]
  if ((!is.null(na.max)) && (sum(is.na(values)) > na.max)) {
    return(NA)
  }
  return(fun(values, ...))
}

raster.aggregate.surroundings <- function(x, fun = mean, na.max = NULL, ...) {
  return(terra::focal(x, w = 3, fun = function(w) aggregate.surroundings(w, fun, na.max, na.rm = T, ...), na.rm = T))
}

raster.extend <- function(x, y, snap = "out", ...) {
  extent <- ext(y)
  if (snap == "out") {
    ssx <- c(xmin(x), ymin(x))
    ssy <- c(xmin(extent), ymin(extent))
    ndx <- c(xmax(x), ymax(x))
    ndy <- c(xmax(extent), ymax(extent))
    extent.ss <- ssx + ((ssy - ssx) %/% res(x)) * res(x)
    extent.nd <- ndx + ((ndy - ndx) %/% res(x) + 1) * res(x)
    extent <- ext(extent.ss[1], extent.nd[1], extent.ss[2], extent.nd[2])
  }
  return(terra::extend(x, extent, ...))
}

raster.resample <- function(y, x, ...) {
  result <- rast(nrows = nrow(x), ncols = ncol(x), extent = ext(x), nlyrs = nlyr(x))
  areay <- prod(res(y))
  stepx <- res(x) * c(1., -1.)
  stepy <- res(y) * c(1., -1.)

  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                   total = length(seq(ymax(x), ymin(x) - stepx[2], by = stepx[2])),
                                   complete = "=",   # Completion bar character
                                   incomplete = "-", # Incomplete bar character
                                   current = ">",    # Current bar character
                                   clear = FALSE,    # If TRUE, clears the bar when finish
                                   width = 100)
  ic <- 1
  for (nsyx in seq(ymax(x), ymin(x) - stepx[2], by = stepx[2])) {
    pb$tick()
    for (nsxx in seq(xmin(x), xmax(x) - stepx[1], by = stepx[1])) {
      covery <- terra::crop(y, ext(nsxx, nsxx + stepx[1], nsyx + stepx[2], nsyx), snap = "out")
      nsyvertices <- matrix(nrow = 2, ncol = prod(dim(covery)[1:2]))
      iv <- 1
      for (nsy in c(nsyx, seq(ymax(covery) + stepy[2], ymin(covery) - stepy[2], by = stepy[2]))) {
        for (nsx in c(nsxx, seq(xmin(covery) + stepy[1], xmax(covery) - stepy[1], by = stepy[1]))) {
          nsyvertices[, iv] <- c(nsx, nsy)
          iv <- iv + 1
        }
      }
      sdyvertices <- matrix(nrow = 2, ncol = prod(dim(covery)[1:2]))
      iv <- 1
      for (sdy in c(seq(ymax(covery) + stepy[2], ymin(covery) - stepy[2], by = stepy[2]), nsyx + stepx[2])) {
        for (sdx in c(seq(xmin(covery) + stepy[1], xmax(covery) - stepy[1], by = stepy[1]), nsxx + stepx[1])) {
          sdyvertices[, iv] <- c(sdx, sdy)
          iv <- iv + 1
        }
      }
      areas <- abs(apply(sdyvertices - nsyvertices, 2, prod))
      result[ic] <- values(terra::aggregate(covery, dim(covery)[1:2], w = areas / areay, na.rm = F))
      ic <- ic + 1
    }
  }
  return(result)
}
