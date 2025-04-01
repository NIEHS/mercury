#' @title Validate station for anomaly calculation
#' @description Check if a station is valid for anomaly calculation
#' @param site_id The station ID
#' @param var The variable to check
#' @return A boolean indicating if the station is valid
#' @export
#' @author Eva Marques
check_ref_period <- function(site_id, var) {
  # Open the GHCN inventory file
  url <- paste0(
    "https://www.ncei.noaa.gov/pub/data/ghcn/daily/",
    "ghcnd-inventory.txt"
  )
  inv <- read.table(url,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(inv) <- c(
    "site_id",
    "lat",
    "lon",
    "var",
    "ts",
    "te"
  )
  start_year <- inv[which(inv$site_id == site_id & inv$var == var), "ts"]
  end_year <- inv[which(inv$site_id == site_id & inv$var == var), "te"]
  start_year <= 1991 & end_year >= 2020
}

#' @title List valid stations
#' @description List all valid stations for anomaly calculation
#' @param var The variable to check
#' @return A data frame with the list of valid stations
#' @export
#' @author Eva Marques
list_valid_stations <- function(var) {
  # Open the GHCN inventory file
  url <- paste0(
    "https://www.ncei.noaa.gov/pub/data/ghcn/daily/",
    "ghcnd-inventory.txt"
  )
  inv <- read.table(url,
    header = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(inv) <- c(
    "site_id",
    "lat",
    "lon",
    "var",
    "ts",
    "te"
  )
  # Get the list of valid stations
  valid_stations <- inv[which(
    inv$var == var & inv$ts <= 1991 & inv$te >= 2020
  ), c("site_id", "lat", "lon", "ts", "te")]
  valid_stations
}


#' Find nearest valid GHCND station to a given location
#' @param lat Latitude
#' @param lon Longitude
#' @return A list with the nearest station for TMAX, TMIN and TAVG
#' @importFrom sf st_as_sf st_point st_sf st_nearest_feature
#' @author Eva Marques
#' @export
find_nearest_valid_ghcnd <- function(lat, lon) {
  valid_tmax <- list_valid_stations("TMAX") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  valid_tmin <- list_valid_stations("TMIN") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  valid_tavg <- list_valid_stations("TAVG") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  my_point <- sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_sf()
  nearest_tmax <- sf::st_nearest_feature(my_point, valid_tmax)
  nearest_tmin <- sf::st_nearest_feature(my_point, valid_tmin)
  nearest_tavg <- sf::st_nearest_feature(my_point, valid_tavg)
  r <- list(
    nearest_tmax = valid_tmax[nearest_tmax, ],
    nearest_tmin = valid_tmin[nearest_tmin, ],
    nearest_tavg = valid_tavg[nearest_tavg, ]
  )
  r
}

#' Open and format GHCNd station
#' @param site_id character. ID of the station.
#' @return data.frame of GHCNh station data.
#' @importFrom lubridate yday
#' @author Eva Marques
#' @export
open_station <- function(site_id) {
  url <- paste0(
    "https://www.ncei.noaa.gov/data/",
    "global-historical-climatology-network-daily/access/",
    site_id,
    ".csv"
  )
  f <- read.csv(url)
  f$TAVG <- 0.1 * f$TAVG # default unit is tenths of degree C
  f$TMAX <- 0.1 * f$TMAX # default unit is tenths of degree C
  f$TMIN <- 0.1 * f$TMIN # default unit is tenths of degree C
  f$DATE <- as.Date(f$DATE, format = "%Y-%m-%d")
  f$yday <- lubridate::yday(f$DATE)
  f
}
