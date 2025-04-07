#' @title map_observations_hw
#' @description This function creates two maps with the observations
#' from Heatwatch and CWS (minimum and maximum temperatures).
#' @param cws A data frame with the observations from CWS.
#' @param hw_r_am A raster with the Heatwatch data for the morning (6-7am).
#' @param hw_r_af A raster with the Heatwatch data for the afternoon (3-4pm).
#' @importFrom scales label_number
#' @importFrom dplyr between
#' @importFrom terra minmax
#' @importFrom tidyterra geom_spatraster scale_fill_whitebox_c
#' @import ggplot2
#' @import ggspatial
#' @return A list with two ggplot objects.
map_cws_hw <- function(
  cws,
  hw_r_am,
  hw_r_af
) {
  geometry <- temp_cal <- network <- NULL
  ts_am <- as.POSIXct(
    "2021-07-23 06:00:00",
    tz = "America/New_York"
  )
  te_am <- as.POSIXct(
    "2021-07-23 06:59:00",
    tz = "America/New_York"
  )
  ts_af <- as.POSIXct(
    "2021-07-23 15:00:00",
    tz = "America/New_York"
  )
  te_af <- as.POSIXct(
    "2021-07-23 15:59:00",
    tz = "America/New_York"
  )
  shape_values <- c("WU" = 21, "PA" = 23)
  cws_am <- cws[which(dplyr::between(cws$time, ts_am, te_am)), ]
  cws_af <- cws[which(dplyr::between(cws$time, ts_af, te_af)), ]
  tn_am <- floor(min(terra::minmax(hw_r_am)[1], min(cws_am$temp_cal)))
  tn_af <- floor(min(terra::minmax(hw_r_af)[1], min(cws_af$temp_cal)))
  tx_am <- ceiling(max(terra::minmax(hw_r_am)[2], max(cws_am$temp_cal)))
  tx_af <- ceiling(max(terra::minmax(hw_r_af)[2], max(cws_af$temp_cal)))

  p_am <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = hw_r_am) +
    ggplot2::geom_sf(
      data = cws_am,
      ggplot2::aes(geometry = geometry, fill = temp_cal, shape = network),
      size = 2
    ) +
    tidyterra::scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      n.breaks = 12,
      limits = c(tn_am, tx_am),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::scale_shape_manual(values = shape_values) +
    ggplot2::labs(
      title = ts_am,
      caption = expression(
        italic("Source: Heatwatch and CWS processed with brassens")
      ),
      fill = "Temperature (C)",
      shape = "Network"
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )

  p_af <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = hw_r_af) +
    ggplot2::geom_sf(
      data = cws_af,
      ggplot2::aes(geometry = geometry, fill = temp_cal, shape = network),
      size = 2
    ) +
    tidyterra::scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      labels = scales::label_number(suffix = paste0("C")),
      n.breaks = 12,
      limits = c(tn_af, tx_af),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::labs(
      title = ts_af,
      caption = expression(
        italic("Source: Heatwatch and CWS processed with brassens")
      ),
      fill = "Temperature (C)",
      shape = "Network"
    ) +
    ggplot2::scale_shape_manual(values = shape_values) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  list("map_am" = p_am, "map_af" = p_af)
}
