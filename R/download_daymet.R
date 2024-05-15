#' Download Daymet data for entire US a given year
#' @param storage_path character path to store the data
#' @param year an integer
#' @param polygon a polygon
download_daymet_poly <- function(storage_path, year, polygon, var) {
  stopifnot("var must be 'tmin' or 'tmax'" = var %in% c("tmin", "tmax"))
  if (!dir.exists(storage_path)) {
    dir.create(storage_path)
  }
  polygon <- format_area(polygon)
  ext <- sf::st_bbox(polygon)
  loc <- c(ext[4], ext[1], ext[2], ext[3])
  file <- paste0(storage_path, "/", var, "_daily_", year, "_ncss.nc")
  if (!file.exists(file)) {
    cat(file, " does not exist \n")
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
  return (y)
}

#' Load daymet data for a given date and area from storage folder
#' Daymet yearly data is downloaded from here
#' https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2129
#' and file name is not changed.
#' @param date a date
#' @param area a polygon
#' @param var a character
#' @param storage_folder a character
#' @return a raster
#' @export
#' @author Eva Marques
load_daymet <- function(date, area, var, storage_folder) {
  stopifnot("var must be 'tmin' or 'tmax'" = var %in% c("tmin", "tmax"))
  yday <- lubridate::yday(date)
  year <- lubridate::year(date)
  # open daymet
  daymet <- terra::rast(paste0(storage_folder,
                               "/daymet_v4_daily_na_", var, "_", year, ".nc"))
  d <- daymet[[yday]]
  area <- format_area(area) |>
    sf::st_transform(crs = terra::crs(d))
  d <- crop_product(terra::vect(area), d)
  return(d)
}
