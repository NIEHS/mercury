#' @title Return air temperature products from 3 different sources for a city:
#' gridmet, daymet, and heatwatch
#' @param city a character
#' @param hw_dict a data.frame
#' @param gridmet_dir a character
#' @param daymet_dir a character
#' @param moment_of_day a character: "am" or "af"
load_temp_products_city <- function(city,
                                    hw_dict,
                                    gridmet_dir,
                                    daymet_dir,
                                    moment_of_day) {
  col <- c("city", "rast_file", "trav_file", "moment_of_day", "ts", "te", "day")
  stopifnot(
    "moment_of_day should be am or af" =
      moment_of_day %in% c("am", "af"),
    "hw_dict doesn't have the right columns" =
      all(col %in% colnames(hw_dict)),
    "city not found" = city %in% hw_dict$city,
    "gridmet_dir not found" = dir.exists(gridmet_dir),
    "daymet_dir not found" = dir.exists(daymet_dir)
  )
  if (is.na(hw_dict[which(hw_dict$city == city & hw_dict$moment_of_day == moment_of_day), ]$rast_file)) {
    message(paste0("No heatwatch data for ", city, " - ", moment_of_day))
    return(NULL)
  } else {
    products <- load_heatwatch_city(hw_dict, city)
    if (moment_of_day == "am") {
      day <- products$day_am
      # ---- heatwatch
      hw_r <- products$r_am |>
        convert_temp(from = "F", to = "C")
      hw_t <- products$t_am
      poly_hw <- terra::as.polygons(0.01*products$r_am)
      hw_area <- format_area(poly_hw)
      # ---- gridmet
      gridmet <- load_gridmet_day(day, gridmet_dir, "tmmn") |>
        terra::project(terra::crs(hw_r)) |>
        crop_product(area_shp = terra::vect(hw_area)) |>
        convert_temp(from = "K", to = "C")
      # ---- daymet
      daymet <- load_daymet(day, hw_area, "tmin", daymet_dir) |>
        terra::project(terra::crs(hw_r))
    }
    if (moment_of_day == "af") {
      day <- products$day_af
      # ---- heatwatch
      hw_r <-products$r_af |>
        convert_temp(from = "F", to = "C")
      hw_t <-products$t_af
      poly_hw <- terra::as.polygons(0.01*products$r_af)
      hw_area <- format_area(poly_hw)
      # ---- gridmet
      gridmet <- load_gridmet_day(day, gridmet_dir, "tmmx") |>
        terra::project(terra::crs(hw_r)) |>
        crop_product(area_shp = terra::vect(hw_area)) |>
        convert_temp(from = "K", to = "C")
      # ---- daymet
      daymet <- load_daymet(day, hw_area, "tmax", daymet_dir) |>
        terra::project(terra::crs(hw_r))
    }
    if ("temp_f" %in% names(hw_t)) {
      hw_t$temp_c <- sapply(hw_t$temp_f,
                            FUN = function(x) {convert_temp(x,
                                                            from = "F",
                                                            to = "C")})
    } else if ("t_f" %in% names(hw_t)) {
      hw_t$temp_c <- sapply(hw_t$t_f,
                            FUN = function(x) {convert_temp(x,
                                                            from = "F",
                                                            to = "C")})
    } else {
      message(paste0("No temperature field found in ", products$t_path))
    }
    hw_t <- terra::project(hw_t, terra::crs(hw_r))
    return(list(gridmet = gridmet,
                daymet = daymet,
                heatwatch_r = hw_r,
                heatwatch_t = hw_t))
  }
}
