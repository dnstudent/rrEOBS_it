library(terra)
library(osmdata)

# Restituisce un oggetto di terra di tipo SpatVector poligonale con il confine regionale principale. Quando si
# fornisce il nome della zona conviene essere molto specifici e settare "featuretype" corrispondentemente. "State" sono
# le nostre regioni.
region.borders <- function(region, crs, featuretype = "State", ...) {
  borders <- osmdata::getbb(region, featuretype = featuretype, format_out = "polygon", ...)
  
  # A volte getbb("polygon") restituisce una lista di confini: in genere il primo è quello continuo più grande,
  # mentre gli altri sono minchietti (isolette, paesi esclusi ecc... Tecnicamente andrebbe controllato) 
  if (class(borders)[1] == "list") {
    borders <- borders[[1]]
  }
  return(terra::vect(borders, type = "polygon", crs = crs))
}

# region può essere il nome della regione oppure i confini in formato SpatVector
region.crop.out <- function(x, region, trim = TRUE, ...) {
  if (class(region) == "character") {
    region <- region.borders(region, crs(x), ...)
  }
  x <- terra::mask(x, region)
  if (trim) {
    x <- terra::trim(x)
  }
  return(x)
}


