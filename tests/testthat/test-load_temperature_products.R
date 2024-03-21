test_that("Check crop_products works", {
  area_shp <- terra::vect(paste0(testthat::test_path("../..", "input/", ""),
                                 "Raleigh_City_Council_Districts/",
                                 "Raleigh_City_Council_Districts.shp"))
})
