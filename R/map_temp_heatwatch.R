
#' Plot heatwatch air temperature
#' @param r A raster object
#' @param borders A polygon
#' @param temp_unit A character string (either "C" or "F") indicating
#' the unit of r
#' @return A ggplot2 object
#' @import ggplot2
#' @import ggspatial
#' @import scales
#' @import tidyterra
map_temp_heatwatch <- function(r, borders, temp_unit) {
  if (is.null(borders)) {
    p <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = r) +
      tidyterra::scale_fill_whitebox_c(
        palette = "muted",
        labels = scales::label_number(suffix = paste0("º", temp_unit)),
        n.breaks = 12,
        limits = c(floor(r@cpp$range_min),
                   ceiling(r@cpp$range_max)),
        guide = guide_legend(reverse = TRUE)
      ) +
      labs(
        fill = "",
        title = "HeatWatch campaign"
      ) +
      ggspatial::annotation_scale(
        location = "bl", pad_x = unit(1, "cm"),
        pad_y = unit(1, "cm"),
        height = unit(0.30, "cm"),
        text_cex = 1
      ) +
      ggspatial::annotation_north_arrow(
        location = "br",
        which_north = "true",
        pad_x = unit(0.2, "cm"),
        pad_y = unit(0.2, "cm")
      ) +
      theme(
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey")
      )
  } else {
    p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    ggspatial::geom_sf(
      data = sf::st_as_sf(terra::project(borders, terra::crs(r))),
      aes(geometry = geometry),
      colour = "black", linewidth = .3, fill = NA
    ) +
    tidyterra::scale_fill_whitebox_c(
      palette = "muted",
      labels = scales::label_number(suffix = paste0("º", temp_unit)),
      n.breaks = 12,
      limits = c(floor(r@cpp$range_min),
                 ceiling(r@cpp$range_max)),
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      fill = "",
      title = "HeatWatch campaign"
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = unit(1, "cm"),
      pad_y = unit(1, "cm"),
      height = unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = unit(0.2, "cm"),
      pad_y = unit(0.2, "cm")
    ) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  }
  return(p)
}
