
#' Plot GridMET air temperature
#' @param r A raster object
#' @param borders A polygon
#' @param temp_unit A character string (either "C" or "F") indicating
#' the unit of r
#' @return A ggplot2 object
#' @import ggplot2
#' @import ggspatial
#' @import scales
#' @import tidyterra
map_temp_gridmet <- function(r, borders = NULL, temp_unit) {
  geometry <- NULL
  if (is.null(borders)) {
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r) +
      tidyterra::scale_fill_whitebox_c(
        palette = "muted",
        labels = scales::label_number(suffix = paste0("º", temp_unit)),
        n.breaks = 12,
        limits = c(floor(terra::minmax(r)[1]),
                   ceiling(terra::minmax(r)[2])),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::labs(
        fill = "",
        title = "GridMET product"
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
      ggspatial::geom_sf(
        data = sf::st_as_sf(terra::project(borders, terra::crs(r))),
        ggplot2::aes(geometry = geometry),
        colour = "black", linewidth = .3, fill = NA
      ) +
      tidyterra::scale_fill_whitebox_c(
        palette = "muted",
        labels = scales::label_number(suffix = paste0("º", temp_unit)),
        n.breaks = 12,
        limits = c(
          floor(terra::minmax(r)[1]),
          ceiling(terra::minmax(r)[2])
        ),
        guide = ggplot2::guide_legend(reverse = TRUE)
      ) +
      ggplot2::labs(
        fill = "",
        title = "GridMET product"
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
