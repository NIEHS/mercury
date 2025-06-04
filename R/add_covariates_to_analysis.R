#' Add imperviousness to gridded product.
#' @description Add imperviousness to gridded product.
#' @param products list. List of gridded products.
#' @param imp_path character. Path to imperviousness raster.
#' @importFrom terra rast buffer crs project
#' @importFrom sf st_as_sf st_geometry
#' @importFrom exactextractr exact_extract
#' @author Eva Marques
#' @export
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

#' Add National Land Cover Classes to gridded product.
#' @description Add National Land Cover Classes to gridded product.
#' @param products list. List of gridded products.
#' @param nlcd_path character. Path to NLCD raster.
#' @importFrom terra rast buffer crs project
#' @importFrom sf st_as_sf st_geometry
#' @importFrom exactextractr exact_extract
#' @author Eva Marques
#' @export
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
