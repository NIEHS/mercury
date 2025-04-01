#' Crop a temperature product
#' @param area_shp a terra::SpatVector. Polygon of the desired cropped area.
#' @param product a terra::SpatRaster or terra::SpatVector.
#' of a temperature product
#' @return the product cropped
#' @importFrom terra project crs crop
#' @export
#' @author Eva Marques
crop_product <- function(area_shp, product) {
  ext <- area_shp |>
    terra::project(terra::crs(product))
  if (class(product)[[1]] == "SpatRaster") {
    cropped <- terra::crop(product, ext, mask = TRUE, snap = "out")
  } else {
    cropped <- terra::crop(product, ext, snap = "out")
  }
  cropped
}
