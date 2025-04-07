#' @title map_temp_products_city
#' @description Create a map with the temperature products for a city
#' @param city character. City name
#' @param hw_dict data.frame. Heatwatch dictionary
#' @param gridmet_dir character. Path to the GRIDmet data
#' @param daymet_dir character. Path to the Daymet data
#' @param moment_of_day character. Moment of the day (am or af)
#' @return ggplot object
#' @export
#' @import ggplot2
#' @import terra
#' @importFrom scales label_number
#' @importFrom ggpubr ggarrange
#' @import tidyterra
map_temp_products_city <- function(
  city,
  hw_dict,
  gridmet_dir,
  daymet_dir,
  moment_of_day
) {
  x <- load_temp_products_city(
    city,
    hw_dict,
    gridmet_dir,
    daymet_dir,
    moment_of_day
  )
  if (is.null(x$heatwatch_r)) {
    NULL
  } else {
    ext <- terra::ext(x$gridmet)
    poly <- terra::as.polygons(0.01 * x$heatwatch_r)
    mint <- floor(min(
      terra::minmax(x$gridmet)[1],
      terra::minmax(x$daymet)[1],
      terra::minmax(x$heatwatch_r)[1]
    ))
    maxt <- ceiling(max(
      terra::minmax(x$gridmet)[2],
      terra::minmax(x$daymet)[2],
      terra::minmax(x$heatwatch_r)[2]
    ))
    map_hw <- map_temp_heatwatch(
      r = x$heatwatch_r,
      tr = x$heatwatch_t,
      borders = poly,
      temp_unit = "C"
    ) +
      tidyterra::scale_fill_whitebox_c(
        palette = "bl_yl_rd",
        labels = scales::label_number(suffix = paste0("C")),
        n.breaks = 12,
        limits = c(mint, maxt),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::ggtitle(
        paste0(
          "Heatwatch ",
          ifelse(moment_of_day == "am", "6-7am - ", "3-4pm - "),
          city
        )
      ) +
      ggplot2::coord_sf(
        xlim = c(ext[1], ext[2]),
        ylim = c(ext[3], ext[4]),
        crs = terra::crs(x$gridmet),
        expand = FALSE
      )
    map_gm <- map_temp_gridmet(x$gridmet, borders = poly, temp_unit = "C") +
      tidyterra::scale_fill_whitebox_c(
        palette = "bl_yl_rd",
        labels = scales::label_number(suffix = paste0("C")),
        n.breaks = 12,
        limits = c(mint, maxt),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::ggtitle(
        paste0(
          "GRIDmet ",
          ifelse(moment_of_day == "am", "tmmn - ", "tmmx - "),
          city
        )
      ) +
      ggplot2::coord_sf(
        xlim = c(ext[1], ext[2]),
        ylim = c(ext[3], ext[4]),
        crs = terra::crs(x$gridmet),
        expand = FALSE
      )
    map_dm <- map_temp_daymet(x$daymet, borders = poly, temp_unit = "C") +
      tidyterra::scale_fill_whitebox_c(
        palette = "bl_yl_rd",
        labels = scales::label_number(suffix = paste0("C")),
        n.breaks = 12,
        limits = c(mint, maxt),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::ggtitle(
        paste0(
          "Daymet ",
          ifelse(moment_of_day == "am", "tmin - ", "tmax - "),
          city
        )
      ) +
      ggplot2::coord_sf(
        xlim = c(ext[1], ext[2]),
        ylim = c(ext[3], ext[4]),
        crs = terra::crs(x$gridmet),
        expand = FALSE
      )
    maps <- ggpubr::ggarrange(map_gm,
      map_dm,
      map_hw,
      ncol = 3,
      common.legend = TRUE,
      legend = "right"
    )
    maps
  }
}
