season2_names <- c("Fredda", "Calda")
season4_names <- c("Inverno", "Primavera", "Estate", "Autunno")
season_names <- function(ids, seasons) {
  n <- if (seasons == 2) {
    season2_names
  } else if (seasons == 4) {
    season4_names
  } else {
    stop("Macché")
  }
  return(n[(ids) %% seasons + 1])
}

season_shift <- function(date, seasons) {
  shift <- if (seasons == 2) {
    2
  } else if (seasons == 4) {
    0
  } else {
    stop("Macché")
  }
  return(format(date, "%m") %>% as.numeric()) %/% (12 / seasons)
}

index.season <- function(dates, seasons) {
  
}