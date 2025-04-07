#' Download Daymet data for a given year
#' @param storage_path character. Path to data storage location.
#' @param year integer.
#' @param polygon SpatVector. Polygon of the desired area.
#' @param var character. Must be tmin or tmax.
#' @importFrom sf st_bbox
#' @importFrom daymetr download_daymet_ncss
#' @importFrom terra rast
#' @author Eva Marques
#' @export
download_daymet_poly <- function(storage_path, year, polygon, var) {
  stopifnot("var must be 'tmin' or 'tmax'" = var %in% c("tmin", "tmax"))
  if (!dir.exists(storage_path)) {
    dir.create(storage_path)
  }
  polygon <- brassens::format_area(polygon)
  ext <- sf::st_bbox(polygon)
  loc <- c(ext[4], ext[1], ext[2], ext[3])
  file <- paste0(storage_path, "/", var, "_daily_", year, "_ncss.nc")
  if (!file.exists(file)) {
    message(file, " does not exist \n")
    daymetr::download_daymet_ncss(
      location = loc,
      start = year,
      end = year,
      frequency = "daily",
      param = var,
      path = storage_path,
      silent = FALSE
    )
  }
  y <- terra::rast(file)
  y
}

#' Load daymet data for a given set of dates and area from storage folder
#' Daymet yearly data is downloaded from here
#' https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2129
#' and file name is not changed.
#' @param dates list. Contains all desired dates.
#' @param area a polygon
#' @param var character.
#' @param storage_folder character. Path to daymet data file.
#' @return a raster
#' @importFrom lubridate yday
#' @importFrom terra rast crs vect
#' @importFrom sf st_transform
#' @export
#' @author Eva Marques
load_daymet <- function(dates, area, var, storage_folder) {
  stopifnot("var must be 'tmin' or 'tmax'" = var %in% c("tmin", "tmax"))
  yday <- lubridate::yday(dates)
  year <- lubridate::year(dates)
  # open daymet
  daymet <- terra::rast(
    paste0(
      storage_folder,
      "/",
      var,
      "_daily_",
      year,
      "_ncss.nc"
    )
  )
  d <- daymet[[yday]]
  area <- brassens::format_area(area) |>
    sf::st_transform(crs = terra::crs(d))
  d <- crop_product(terra::vect(area), d)
  d
}
