add_imp <- function(products, imp_path) {
  stopifnot(all(c("heatwatch_t" %in% names(products))))
  p <- products
  imp <- terra::rast(imp_path)
  bufs_pol <- terra::buffer(p$heatwatch_t, width = 400) |>
    terra::project(terra::crs(imp)) |>
    sf::st_as_sf()
  p$heatwatch_t$imp <- exactextractr::exact_extract(
    x = imp,
    y = sf::st_geometry(bufs_pol),
    fun = "mode"
  )
  p
}

add_nlcd <- function(products, nlcd_path) {
  stopifnot(all(c("heatwatch_t" %in% names(products))))
  p <- products
  nlcd <- terra::rast(nlcd_path)
  bufs_pol <- terra::buffer(p$heatwatch_t, width = 400) |>
    terra::project(terra::crs(nlcd)) |>
    sf::st_as_sf()
  p$heatwatch_t$nlcd <- exactextractr::exact_extract(
    x = nlcd,
    y = sf::st_geometry(bufs_pol),
    fun = "mode"
  )
  p
}
