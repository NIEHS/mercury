#' Plot heatwatch air temperature
#' @param r A spatraster of heatwatch air temperature
#' @param tr A spatvector of heatwatch transects
#' @param borders A polygon
#' @param temp_unit A character string (either "C" or "F") indicating
#' the unit of r
#' @return A ggplot2 object
#' @import ggplot2
#' @import ggspatial
#' @import scales
#' @import tidyterra
map_temp_heatwatch <- function(r, tr, borders = NULL, temp_unit) {
  temp_c <- geometry <- NULL
  if (is.null(borders)) {
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r) +
      tidyterra::geom_spatvector(
        data = tr,
        ggplot2::aes(fill = temp_c),
        shape = 21,
        stroke = 0.005
      ) +
      tidyterra::scale_fill_whitebox_c(
        palette = "muted",
        labels = scales::label_number(suffix = paste0(temp_unit)),
        n.breaks = 12,
        limits = c(
          floor(r@cpp$range_min),
          ceiling(r@cpp$range_max)
        ),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::labs(
        fill = "",
        title = "HeatWatch campaign"
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
  } else {
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r) +
      tidyterra::geom_spatvector(
        data = tr,
        ggplot2::aes(fill = temp_c),
        shape = 21,
        stroke = 0.005
      ) +
      ggspatial::geom_sf(
        data = sf::st_as_sf(terra::project(borders, terra::crs(r))),
        ggplot2::aes(geometry = geometry),
        colour = "black", linewidth = .3, fill = NA
      ) +
      tidyterra::scale_fill_whitebox_c(
        palette = "muted",
        labels = scales::label_number(suffix = paste0(temp_unit)),
        n.breaks = 12,
        limits = c(
          floor(r@cpp$range_min),
          ceiling(r@cpp$range_max)
        ),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::labs(
        fill = "",
        title = "HeatWatch campaign"
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
  }
  p
}
