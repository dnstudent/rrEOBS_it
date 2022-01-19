packages <- c("ggplot2", "tidyr", "lubridate", "zoo", "terra")
invisible(lapply(packages, library, character.only = T))

sources <- c("geo_data.R", "load.R", "plotting.R", "utils.R", "indexer.R")
invisible(lapply(sources, source))

from <- "1961-01-01"
to <- "2010-12-31"

###########################
# LOADING STUFF (from zero)
###########################
# arcis.full <- load.arcis(from, to)
# arcis.full <- rast(paste0(path.arcis.temp, "arcis.tif"), opts = c("NUM_THREADS=ALL_CPUS"))
# print("ArCIS Loaded")
# eobs <- load.eobs(from, to, extent = arcis.full, snap = "out")
# print("EOBS loaded")
# arcis <- raster.extend(arcis.full, eobs, snap = "out")
# print("ArCIS extended")
# writeRaster(arcis, paste0(path.arcis.temp, "arcis.extended.tif"), gdal = c("COMPRESS=NONE"))
# print("ArCIS saved")
# arcis.resampled <- raster.resample(arcis, eobs, na.rm = T)
# print("ArCIS resampled")


############################
# LOADING STUFF (from cache)
############################
arcis <- rast(paste0(path.arcisVeobs, "arcis.nc"))
arcis_resampled <-
  rast(paste0(path.arcisVeobs, "arcis_resampled.nc"))
arcis_cut <- rast(paste0(path.arciscutVeobs, "arcis.nc"))
arcis_resampled_cut <-
  rast(paste0(path.arciscutVeobs, "arcis_resampled.nc"))
eobs <- rast(paste0(path.arcisVeobs, "eobs.nc"))

datasets <- list(
  "E-OBS" = eobs,
  "ArCIS" = arcis,
  "ArCIS_resampled" = arcis_resampled,
  "ArCIS_cut" = arcis_cut,
  "ArCIS_resampled_cut" = arcis_resampled_cut
)

index <- as.numeric(format(time(eobs), format = "%m"))
runs <- rle(index)$lengths

# Expects a raster with time attribute, starting from 01-01 and ending with 12-31
# Not a generic function! Beware
# Anomalie giornaliere rispetto alla media mensile
anomalies.daily.month <- function(x) {
  means <- x %>%
    #raster.time.reduction(c("%Y", "%m"), "mean", na.rm = T) %>% # daily mean on month
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(length(runs) %/% 12) %>%
    rep(runs) # disaggregate: replicate monthly means for each day
  return(x / means)
}

anomalies.monthly <- function(x) {
  monthly_cumulatives <- raster.time.reduction(x, c("%Y", "%m"), "sum", na.rm = T)
  means <- monthly_cumulatives %>% 
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(length.out = monthly_cumulatives)
  return(monthly_cumulatives / means)
}

# shift: quanti mesi spostare all'anno successivo
# Anomalie giornaliere rispetto alla media stagionale
anomalies.daily.season <- function(x, seasons = 4) {
  index.y <- time(x) %>% format("%Y") %>% as.numeric() * seasons
  # - 1: normalizza gli indici dei mesi in modo da partire da 0
  index.s <- index.y + season_shift(seasons)
  seasons_rle <- rle(index.s)
  n_seasons <- length(seasons_rle$lengths)
  
  means <- terra::tapp(x, index.s, "mean", na.rm = T) %>%
    terra::tapp(index = 1:seasons, "mean", na.rm = T) %>%
    rep(length.out = n_seasons) %>%
    rep(seasons_rle$lengths)
  return(x / means)
}

# Anomalie della media mensile
anomalies.monthly.mean <- function(x, map = FALSE) {
  monthly_precipitations <-
    raster.time.reduction(x, c("%Y", "%m"), "sum", na.rm = T)
  means <- monthly_precipitations %>%
    raster.time.reduction(c("%m"), "mean", na.rm = T) %>% # monthly means
    rep(length.out = nlyr(monthly_precipitations))
  if (map) return(monthly_precipitations / means)
  a <-
    terra::global(monthly_precipitations / means, "mean", na.rm = T)
  names(a) <- "Anomalia media mensile"
  a$Data <-
    names(monthly_precipitations) %>% paste0("01") %>% as_date(format = "X%Y%m%d")
  return(a)
}

# Anomalie della media stagionale
anomalies.seasonal.mean <-
  function(x,
           seasons = 4,
           map = FALSE) {
    index.y <- time(x) %>% format("%Y") %>% as.numeric() * seasons
    # - 1: normalizza gli indici dei mesi in modo da partire da 0
    index.s <- index.y + season_shift(time(x), seasons)
    seasons_rle <- rle(index.s)
    n_seasons <- seasons_rle$lengths %>% length()
    
    seasonal_precipitations <-
      terra::tapp(x, index.s, "mean", na.rm = T)
    means <- seasonal_precipitations %>%
      terra::tapp(
        index = names(seasonal_precipitations) %>%
          substring(2) %>% as.numeric() %>% season_id(seasons),
        "mean",
        na.rm = T
      ) %>%
      rep(length.out = n_seasons)
    
    ids <-
      names(seasonal_precipitations) %>% substring(2) %>% as.numeric()
    
    if (map) {
      a <- seasonal_precipitations / means
      names(a) <- paste0(season_names(ids, seasons), "_", names(seasonal_precipitations) %>% substring(2) %>% as.numeric() %/% seasons)
      return(a)
    } else {
      a <-
        terra::global(seasonal_precipitations / means, "mean", na.rm = T)
      names(a) <- "Anomalia media stagionale"
      a$Anno <- ids %/% seasons + (ids %% seasons) / seasons
      a$Stagione <- season_names(ids, seasons)
    }
    return(a)
  }

compute_anomalies <- function(anomaly_fn, ...) {
  return(lapply(datasets, anomaly_fn, ...))
}

# group_rr <- function(anomalies, nclasses = 10) {
#   terra::classify(anomalies,
#                   c(-1, 0, 1 / rev(1:(nclasses / 2)), 2:(nclasses / 2), Inf),
#                   include.lowest = F,
#                   right = T) %>%
#     values(mat = F) %>%
#     return()
# }


for (region in c("Emilia-Romagna", "Toscana", "Veneto", "Trentino-Alto Adige")) {
  # Regional stuff
  region1 <- paste0(region, ", Italia")
  
  regional_seasonal_trends <- lapply(datasets,
                                      function(x)
                                        region.crop(x, region1, crop.out = T) %>%
                                        anomalies.seasonal.mean(4)) %>% dplyr::bind_rows(.id = "Dataset")
  pdf(paste0(path.results, "seasonal_trends/", region, ".pdf"))
  print(ggplot(data = regional_seasonal_trends,
         mapping = aes(x = Anno, y = `Anomalia media stagionale`, group = Dataset, color = Dataset)) +
    geom_line(aes(linetype = Dataset)) +
      ggtitle(paste("Trend", region)))
  dev.off()
  
  regional_monthly_maps <- lapply(datasets,
                                   function(x)
                                     region.crop(x, region1, crop.out = T) %>% 
                                     anomalies.daily.month %>% values(mat = F))
  for (name in names(regional_monthly_maps)) {
    d <- regional_monthly_maps[[name]]
    d <- d[!is.na(d) & 1/20 < d & d < 20]
    pdf(paste0(path.results, "daily_seasonal_anomalies/", region,"_", name, ".pdf"), pointsize = 6)
    hist(
      d,
      freq = F,
      main = paste(name, region),
      breaks = 60
    )
    dev.off()
  }
}
gc()

ans <- eobs %>% region.crop("Emilia-Romagna", crop.out = T) %>% anomalies.monthly()
for (i in 1:12) {
  indices <- which(time(ans) %>% format("%m") %>% as.numeric() == i)
  r <- terra::subset(ans, indices)
  v <- values(r, mat = F)
  v <- v[!is.na(v) & 1/20 < v & v < 20]
  hist(v, freq = F, main = month.name[i], breaks = 40)
}




