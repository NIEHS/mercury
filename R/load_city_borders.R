#' Load city borders
#' @param city The city to load the borders for
#' @param us_cities_shp The path to the US cities shapefile
#' @return a vector with the city borders
#' @export
#' @author Eva Marques
#' (see Urban Areas .zip on www.census.gov)
load_city_borders <- function(city, us_cities_shp) {
  stopifnot("us_cities_shp does not exist" = file.exists(us_cities_shp))
  us_cities <- terra::vect(us_cities_shp)
  city_name_shp <- grep(
    pattern = sub(pattern = "_", replacement = " ", city),
    ignore.case = TRUE,
    value = TRUE,
    us_cities$NAME10
  )
  us_cities[which(us_cities$NAME10 %in% city_name_shp), ]
}
