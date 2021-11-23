library(colorspace)
library(dplyr)
library(purrr)
library(terra)

source("load.R")
source("plotting.R")
source("station_data.R")
source("utils.R")

clino.monthly <- load.clino("monthly")

clino.seasonal <- terra::tapp(clino.monthly,
                              index = c(rep(0, 3), rep(1, 6), rep(0, 3)),
                              fun = sum, na.rm = T) %>% 
  raster.aggregate(12, mean, na.rm = T, crop = T)
names(clino.seasonal) <- c("Cold", "Warm")

eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>%
  terra::subset(seq(as.Date("1961-04-01"), as.Date("1990-09-30"), by = "day") %>%
                  format("%Y-%m-%d"))

index.y <- (substr(names(eobs), 1, 4) %>% as.numeric() - 1961) * 2
index.s <- (substr(names(eobs), 6, 7) %>% as.numeric() + 2) %/% 6
eobs.seasonal <- eobs %>%
  terra::tapp(index = index.y + index.s, fun = sum, na.rm = T) %>%
  terra::tapp(index = rep(c(0, 1)), fun = mean, na.rm = T)
names(eobs.seasonal) <- c("Cold", "Warm")

ext(clino.seasonal) <- ext(eobs.seasonal)

correction <- clino.seasonal - eobs.seasonal

pdf(paste0(path.results, "seasonal.pdf"), width = 6, height = 3, pointsize = 6)
title("CLINO - EOBS")
plot.correction(correction)
dev.off()
