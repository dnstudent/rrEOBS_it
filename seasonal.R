library(colorspace)
library(dplyr)
library(purrr)
library(terra)

source("load.R")
source("plotting.R")
source("station_data.R")
source("utils.R")

clino <- load.clino("monthly")

clino <- terra::tapp(clino,
  index = c(rep(0, 3), rep(1, 6), rep(0, 3)),
  fun = sum, na.rm = T) %>%
  raster.aggregate(12, mean, na.rm = T, crop = T)
names(clino) <- c("Cold", "Warm")

eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>%
  terra::subset(seq(as.Date("1961-04-01"), as.Date("1990-09-30"), by = "day") %>%
    format("%Y-%m-%d"))

index.y <- (substr(names(eobs), 1, 4) %>% as.numeric() - 1961) * 2
index.s <- (substr(names(eobs), 6, 7) %>% as.numeric() + 2) %/% 6
eobs <- eobs %>%
  terra::tapp(index = index.y + index.s, fun = sum, na.rm = T) %>%
  terra::tapp(index = rep(c(1, 0)), fun = mean, na.rm = T)
names(eobs) <- c("Warm", "Cold")

ext(clino) <- ext(eobs)

correction <- clino - eobs

pdf(paste0(path.results, "seasonal.pdf"),
    height = ymax(correction) - ymin(correction),
    width = 2*(xmax(correction) - xmin(correction) - 1.2),
    pointsize = 20)
plot.raster(correction, correction = T, range = 300, colNA = "green", mar = c(3.1, 3.1, 2.1, 5.1))
dev.off()

correction.surroundings <- correction %>%
  terra::sapp(raster.aggregate.surroundings, na.max = 4, na.rm = T)
elevs.complexity <- load.elevs.gtopo(extent = eobs) %>%
  raster.aggregate(12, fun = sd, na.rm = T)
names(elevs.complexity) <- "Elevation complexity"


for (region in c("Emilia-Romagna", "Toscana", "Calabria", "Trentino Alto Adige", "Sardegna", "Campania")) {
  borders <- region.cut(paste(region, ", Italy"), featuretype = "State")
  region.correction <- terra::crop(correction, borders)
  # stations <- stations_op(region.complexity)
  
  par(mfrow = c(1, 2))
  plot.regions(region, region.correction, range=c(-200, 200), correction = T, borders = F)
  region.surroundings <- terra::crop(correction.surroundings, borders)
  region.complexity <- terra::crop(elevs.complexity, region.correction)
  boxplot(region.correction$Cold,
          terra::classify(region.complexity, rcl = 10, include.lowest = T),
          ylab = "Precipitation correction [mm]",
          main = paste0(region, " - cold season"))
  boxplot(region.correction$Warm,
          terra::classify(region.complexity, rcl = 10, include.lowest = T),
          ylab = "Precipitation correction [mm]",
          main = paste0(region, " - warm season"))
  # plot(region.raster, region.surroundings, main = paste(region, "correlation"))
  print("######################")
  print(paste0(region, ":"))
  cor_rr <- cor(values(region.surroundings), values(region.correction), use = "na.or.complete")
  print("Surrounding cells correlation")
  print(cor_rr)
  cor_elev <- cor(values(region.correction), values(region.complexity), use = "na.or.complete")
  print("Elevation complexity correlation")
  print(cor_elev)
  print("######################")
}


