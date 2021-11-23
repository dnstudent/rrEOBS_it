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
  fun = sum, na.rm = T
) %>%
  raster.aggregate(12, mean, na.rm = T, crop = T)
names(clino.seasonal) <- c("Cold", "Warm")

eobs <- rast(paste0(path.monthly.temp, "eobs_it_month.tif")) %>%
  terra::subset(seq(as.Date("1961-04-01"), as.Date("1990-09-30"), by = "day") %>%
    format("%Y-%m-%d"))

index.y <- (substr(names(eobs), 1, 4) %>% as.numeric() - 1961) * 2
index.s <- (substr(names(eobs), 6, 7) %>% as.numeric() + 2) %/% 6
eobs.seasonal <- eobs %>%
  terra::tapp(index = index.y + index.s, fun = sum, na.rm = T) %>%
  terra::tapp(index = rep(c(1, 0)), fun = mean, na.rm = T)
names(eobs.seasonal) <- c("Warm", "Cold")

ext(clino.seasonal) <- ext(eobs.seasonal)

correction <- clino.seasonal - eobs.seasonal

pdf(paste0(path.results, "seasonal.pdf"), width = 6, height = 3, pointsize = 6)
plot.correction(correction)
dev.off()

correction.surroundings <- correction %>%
  terra::sapp(raster.aggregate.surroundings, na.max = 4, na.rm = T)
elevs.complexity <- load.elevs.gtopo(extent = eobs.seasonal) %>%
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


