#' Load the heatwatch data for a given city
#' @param city The city to load the data for
#' @param var The variable to load ("am", "af" or "pm")
#' @param heatwatch_dict The path to the heatwatch dictionary file
#' @return A raster with the heatwatch data for the city
#' @export
#' @author Eva Marques
load_heatwatch_city <- function(city, var, heatwatch_dict) {
  stopifnot("heatwatch dictionary file does not exist" =
              file.exists(heatwatch_dict))
  hw_dict <- read.csv(heatwatch_dict)
  stopifnot("city not found in heatwatch dictionary" = city %in% hw_dict$city)
  hw_path <- hw_dict[hw_dict$city == city, "path"]
  stopifnot("heatwatch path does not exist" = dir.exists(hw_path))
  hw_file <- paste0(hw_path, 
                    hw_dict[hw_dict$city == city, paste0(var, "_fname")])
  stopifnot("heatwatch file does not exist" = file.exists(hw_file))
  # Load the data
  output <- terra::rast(hw_file)
  return(output)
}