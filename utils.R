library(progress)
library(terra)

source("geo_data.R")

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
    ymin(x) + (nrow - (nrow %/% 2)) * sy, ymax(x) - (nrow %/% 2) * sy
  )
  return(terra::crop(x, new.ext))
}

raster.aggregate <- function(x, fact, ..., crop = TRUE) {
  if (crop) {
    return(raster.trim(x, trim_size.row(x, fact), trim_size.col(x, fact)) %>% terra::aggregate(fact, ...))
  }
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

#' Transfer values of a SpatRaster to another one with a different geometry
#'
#' @param x SpatRaster to be resampled
#' @param y SpatRaster with the geometry that x should be resampled to
#' @param ... To be implemented
#'
#' @return A resampled SpatRaster
#'
#' @examples
raster.resample <- function(x, y, fun = terra::weighted.mean, ...) {
  data <- x %>% terra::crop(y, snap = "out") %>% raster.extend(y, snap = "out")
  result <- rast(nrows = nrow(y), ncols = ncol(y), extent = ext(y), nlyrs = nlyr(data))
  names(result) <- names(x)
  time(result) <- time(x)
  units(result) <- units(x)
  varnames(result) <- varnames(x)[1]
  
  data.step <- res(data)
  cell.step <- res(result)

  pb <- progress::progress_bar$new(
    format = "[:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = nrow(result),
    complete = "=", # Completion bar character
    incomplete = "-", # Incomplete bar character
    current = ">", # Current bar character
    clear = FALSE, # If TRUE, clears the bar when finish
    width = 100
  )
  ic <- 1
  # cell è una cella del raster risultante: ha le proprietà delle celle di y (risoluzione, extent)
  # invece che "east" si legga "west", ho fatto confusione
  # cell.cover è la "copertura" della cella, ovvero l'insieme più piccolo di celle di x che ricoprono la cella.
  #   Queste forniscono i dati da utilizzare per riempire, in maniera pesata, la cella del raster resampled
  for (cell.north in seq(ymax(result), ymin(result) + cell.step[2], by = -cell.step[2])) {
    pb$tick()
    for (cell.east in seq(xmin(result), xmax(result) - cell.step[1], by = cell.step[1])) {
      cell.extent <- ext(cell.east, cell.east + cell.step[1], cell.north - cell.step[2], cell.north)
      cell.cover <- terra::crop(data, cell.extent, snap = "out")
      # matrix to store all the north-east corners contributing to a resampled cell's value
      cover.ne <- matrix(nrow = 2, ncol = nrow(cell.cover)*ncol(cell.cover))
      iv <- 1
      # I due cicli seguenti popolano due vettori con tutti gli angoli ne e sw
      # nella copertura della cella in esame, in modo da calcolare le aree
      for (areas.north in c(cell.north, seq(ymax(cell.cover) - data.step[2], ymin(cell.cover) + data.step[2], by = -data.step[2]))) {
        for (areas.east in c(cell.east, seq(xmin(cell.cover) + data.step[1], xmax(cell.cover) - data.step[1], by = data.step[1]))) {
          cover.ne[, iv] <- c(areas.east, areas.north)
          iv <- iv + 1
        }
      }
      cover.sw <- matrix(nrow = 2, ncol = nrow(cell.cover)*ncol(cell.cover))
      iv <- 1
      for (areas.south in c(seq(ymax(cell.cover) - data.step[2], ymin(cell.cover) + data.step[2], by = -data.step[2]),
                           cell.north - cell.step[2])) {
        for (areas.west in c(seq(xmin(cell.cover) + data.step[1], xmax(cell.cover) - data.step[1], by = data.step[1]),
                            cell.east + cell.step[1])) {
          cover.sw[, iv] <- c(areas.west, areas.south)
          iv <- iv + 1
        }
      }
      areas <- abs(apply(cover.sw - cover.ne, 2, prod))
      result[ic] <- values(terra::aggregate(cell.cover, c(nrow(cell.cover), ncol(cell.cover)),
                                            fun = fun, w = areas, ...))
      ic <- ic + 1
    }
  }
  return(result)
}

date.groupby <- function(dates, by, is.date = TRUE) {
  if (is.date) {
    return(dates %>%
      format(format = by) %>%
      as.numeric())
  } else {
    return(dates %>%
      substr(by[1], by[2]) %>%
      as.numeric())
  }
}

raster.time.reduction <- function(x, group.by, fun = "sum", ...) {
  index <- date.groupby(time(x), paste0(group.by, collapse = ""), is.date = T)
  r <- terra::tapp(x, index, fun, ...)
  
  n <- names(r) %>% substr(2, 9)
  if (length(group.by) == 1) {
    sub("^([1-9])$", "0\\1", n)
  }
  if (!("%Y" %in% group.by)) {
    n <- paste0(format(time(x)[1], format = "%Y"), n)
  }
  if (!("%m" %in% group.by)) {
    n <- paste0(substr(n, 1, 4), "01", substr(n, 5, 6))
  }
  if (!("%d" %in% group.by)) {
    n <- paste0(substr(n, 1, 6), "01")
  }
  time(r) <- as.Date(n, format = "%Y%m%d")
  return(r)
}

