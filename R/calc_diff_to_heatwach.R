#' Calculate difference to heatwatch product.
#' @description Calculate difference to heatwatch product.
#' @param products list. List of gridded products.
#' @importFrom terra buffer
#' @importFrom sf st_as_sf st_geometry
#' @importFrom exactextractr exact_extract
#' @author Eva Marques
#' @export
add_diff_heatwatch <- function(products) {
  stopifnot(all(c(
    "daymet" %in% names(products),
    "gridmet" %in% names(products),
    "heatwatch_t" %in% names(products)
  )))
  p <- products
  bufs_pol <- terra::buffer(p$heatwatch_t, width = 100) |>
    sf::st_as_sf()
  p$heatwatch_t$daymet <- exactextractr::exact_extract(
    x = p$daymet,
    y = sf::st_geometry(bufs_pol),
    fun = "mode"
  )
  p$heatwatch_t$gridmet <- exactextractr::exact_extract(
    x = p$gridmet,
    y = sf::st_geometry(bufs_pol),
    fun = "mode"
  )
  p$heatwatch_t$delta_daymet <- p$heatwatch_t$daymet - p$heatwatch_t$temp_c
  p$heatwatch_t$delta_gridmet <- p$heatwatch_t$gridmet - p$heatwatch_t$temp_c
  p
}

#' Calculate difference to heatwatch product for every gridded products.
#' @param hw_dict list. Contains list of heatwatch cities.
#' @param gridmet_dir character. Path to gridmet data.
#' @param daymet_dir character. Path to daymet data.
#' @param moment_of_day character. "am" or "af".
#' @param imp_path character. Path to imperviousness data.
#' @param nlcd_path character. Path to National Land Cover Dataset.
#' @param as_df boolean. If TRUE, returns a data.frame.
#' @return Differences calculated for all the products.
#' @author Eva Marques
#' @export
calc_all_diff <- function(
  hw_dict,
  gridmet_dir,
  daymet_dir,
  moment_of_day,
  imp_path,
  nlcd_path,
  as_df = TRUE
) {
  list_cities <- unique(hw_dict$city)
  all_diff <- list()
  for (c in list_cities) {
    cat(c, ":\n")
    products <- load_temp_products_city(
      c,
      hw_dict,
      gridmet_dir,
      daymet_dir,
      moment_of_day
    ) |>
      add_diff_heatwatch() |>
      add_imp(imp_path = imp_path) |>
      add_nlcd(nlcd_path = nlcd_path)
    if (as_df) {
      all_diff[[c]] <- as.data.frame(products$heatwatch_t[, c(
        "temp_c",
        "daymet",
        "gridmet",
        "delta_daymet",
        "delta_gridmet",
        "imp",
        "nlcd"
      )])
    } else {
      all_diff[[c]]$city <- products$heatwatch_t[, c(
        "temp_c",
        "daymet",
        "gridmet",
        "delta_daymet",
        "delta_gridmet",
        "imp",
        "nlcd"
      )]
    }
  }
  all_diff
}
