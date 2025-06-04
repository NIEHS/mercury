test_that("download_daymet_poly works as expected", {
  # Mock inputs
  storage_path <- "../testdata/" |>
    testthat::test_path()
  year <- 2021
  polygon <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(-75, 40.8), c(-75, 41), c(-74.8, 41), c(-74.8, 40.8), c(-75, 40.8)
          )
        )
      ),
      crs = 4326
    )
  )
  var <- "tmin"
  # Run the function
  result <- download_daymet_poly(storage_path, year, polygon, var)
  # Assertions
  expect_true(
    inherits(result, "SpatRaster")
  ) # Check if the result is a SpatRaster
  expect_true(
    file.exists(
      file.path(
        storage_path,
        paste0(var, "_daily_", year, "_ncss.nc")
      )
    )
  )
})

test_that("load_daymet works as expected", {
  # Mock inputs
  storage_path <- "../testdata/" |>
    testthat::test_path()
  var <- "tmin"
  year <- 2021
  file_path <- file.path(
    storage_path,
    paste0(var, "_daily_", year, "_ncss.nc")
  )
  # Ensure the file exists (assumes download_daymet_poly was tested and works)
  if (!file.exists(file_path)) {
    skip(
      "Required file does not exist.
      Ensure download_daymet_poly is tested first."
    )
  }
  # Run the function
  dates <- seq(
    as.Date("2021-01-01"),
    as.Date("2021-01-03"),
    by = "day"
  )
  area <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(
        list(
          rbind(
            c(-75, 40.8), c(-75, 41), c(-74.8, 41), c(-74.8, 40.8), c(-75, 40.8)
          )
        )
      ),
      crs = 4326
    )
  )
  var <- "tmin"
  result <- load_daymet(dates, area, var, storage_path)
  # Assertions
  expect_true(
    inherits(result, "SpatRaster")
  )
  expect_equal(
    terra::nlyr(result),
    3
  )
})
