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

#' @import exactextractr
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
