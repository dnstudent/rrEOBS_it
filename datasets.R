packages <- c("terra", "giscoR")
invisible(lapply(packages, library, character.only = T))

sources <- c("geo_data.R", "load.R", "plotting.R", "utils.R")
invisible(lapply(sources, source))

from <- "1961-01-01"
to <- "2010-12-31"

path <- "/Users/davidenicoli/OneDrive - Università degli Studi di Milano/Uni/Workspace/LabClima/rrEOBS_it/rasters/new/arcisVeobs/"

it <- gisco_get_countries(resolution = "03", country = "Italy") %>% vect()
arcis <- load.arcis(from, to) %>% region.crop(it)
eobs <- load.eobs(from, to, extent = arcis, snap = "near") %>% region.crop(it)
names(eobs) <- time(eobs)
eobs_true_profile <- eobs$`1990-01-01`
eobs <- terra::mask(eobs, eobs_true_profile)
time(arcis) <- time(eobs)

arcis_resampled <- rast(paste0(path.data, "old/arcis.resampled.tif")) %>% region.crop(it) %>% terra::crop(eobs) %>% terra::mask(eobs_true_profile)
time(arcis_resampled) <- time(eobs)
eobs <- terra::mask(eobs, arcis_resampled$rr_1)

arcis_resampled_cut <- terra::ifel(arcis_resampled < 0.5, 0.0, arcis_resampled)
arcis_cut <- terra::ifel(arcis < 0.5, 0.0, arcis)


test_missing <- function(x) {
  plot(subst(mean(x, na.rm = T), NA, 0) - mean(x, na.rm = F))
}

datasets <- list(arcis_resampled_cut, arcis_cut)
dnames <- list("arcis_resampled", "arcis")
for (region in c("Trentino-Alto Adige", "Emilia-Romagna", "Toscana")) {
  region1 <- paste0(region, ", Italy")
  for (i in 1:length(datasets)) {
    r <- region.crop(datasets[[i]], region1)
    writeCDF(r, paste0(path.arciscutVeobs, region, "_", dnames[[i]], ".nc"),
             varname = "rr", longname = longnames(datasets[[i]]), unit = "mm", overwrite = F)
  }
}

