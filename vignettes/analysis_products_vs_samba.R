cities_shp <- "./input/500Cities_City_11082016/CityBoundaries.shp"
cities <- terra::vect(cities_shp)

gridmet_dir <- "./input/gridmet/"
daymet_dir <- "./input/daymet/north_america/"

## Data
cs <- "npw"
if (cs == "npw") {
  # Open samba output on NYC / Philadelphia on January 2021
  samb <- terra::rast(
    "./input/samba/npw_inference_predmean_2021010106_2021013105.tif"
  )
  out_dir <- "./output/npw/"
  phi_nyc <- cities[which(cities$ST %in% c("NJ", "NY", "PA")), ]
  cities <- phi_nyc[which(
    !(
      phi_nyc$NAME %in% c(
        "Buffalo",
        "Pittsburgh",
        "Erie",
        "Rochester",
        "Syracuse",
        "Schenectady",
        "Albany",
        "Scranton",
        "Reading"
      )
    )
  ), ]
  tz_local <- "America/New_York"
} else if (cs == "nps") {
  # Open samba output on NYC / Philadelphia on July 2024
  samb <- terra::rast(
    "./input/samba/nps_inference_predmean_2024070105_2024073104.tif"
  )
  out_dir <- "./output/nps/"
  phi_nyc <- cities[which(cities$ST %in% c("NJ", "NY", "PA")), ]
  cities <- phi_nyc[which(
    !(
      phi_nyc$NAME %in% c(
        "Buffalo",
        "Pittsburgh",
        "Erie",
        "Rochester",
        "Syracuse",
        "Schenectady",
        "Albany",
        "Scranton",
        "Reading"
      )
    )
  ), ]
  tz_local <- "America/New_York"
} else if (cs == "phoe") {
  # Open samba output on Phoenix on July 2023
  samb <- terra::rast(
    "./input/samba/phoe_inference_predmean_2023070109_2023073123.tif"
  )
  out_dir <- "./output/phoe/"
  az <- cities[which(cities$ST %in% c("AZ")), ]
  cities <- az[which(!(az$NAME %in% c("Yuma", "Tucson"))), ]
  tz_local <- "America/Phoenix"
} else if (cs == "tri") {
  # Open samba output on the Triangle on July 2021
  samb <- terra::rast(
    "./input/samba/tri_inference_predmean_2021070105_2021073123.tif"
  )
  out_dir <- "./output/tri/"
  cities <- cities[which(cities$NAME %in% c("Raleigh", "Durham", "Cary")), ]
  tz_local <- "America/New_York"
} else {
  message("cs not found")
  stop()
}

x <- lutz::tz_list()
tz <- x[which(x$tz_name == tz_local & x$is_dst == FALSE), ]$zone

terra::time(samb) <- as.POSIXct(terra::time(samb), tz = "UTC")
# change to local time: TX and TN are calculated
# for each day from 12am - 12am local time
terra::time(samb) <- lubridate::with_tz(terra::time(samb), tz)
ts <- lubridate::date(as.POSIXct(min(terra::time(samb))))
te <- lubridate::date(as.POSIXct(max(terra::time(samb))))
dates <- seq(ts, te, by = "1 day")
ext_shp <- terra::ext(samb) |>
  terra::as.polygons(crs = terra::crs(samb))
xlim <- c(terra::ext(samb)$xmin, terra::ext(samb)$xmax)
ylim <- c(terra::ext(samb)$ymin, terra::ext(samb)$ymax)

# Open gridmet for study period
gm_tmmn <- load_gridmet(dates, ext_shp, "tmmn", gridmet_dir) |>
  convert_temp(from = "K", to = "C")
gm_tmmx <- load_gridmet(dates, ext_shp, "tmmx", gridmet_dir) |>
  convert_temp(from = "K", to = "C")
terra::plot(gm_tmmn[[time(gm_tmmn) == dates[1]]])

# Open daymet for study period
dm_tmin <- load_daymet(dates, ext_shp, "tmin", daymet_dir)
dm_tmax <- load_daymet(dates, ext_shp, "tmax", daymet_dir)
terra::plot(dm_tmin[[time(dm_tmin) == dates[1]]])

# toremove when we will receive data:
if (cs == "nps") {
  dm_tmin <- gm_tmmn
  dm_tmax <- gm_tmmx
  terra::values(dm_tmin) <- NA
  terra::values(dm_tmax) <- NA
}

map_temp <- function(r, borders, tn, tx, xlim, ylim) {
  geometry <- NULL
  gg <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    tidyterra::geom_spatvector(
      data = borders,
      ggplot2::aes(geometry = geometry),
      colour = "black", linewidth = .3, fill = NA
    ) +
    ggplot2::scale_fill_stepsn(
      colours = c(
        "#F7F7F7",
        "#FDDBC7",
        "#F4A582",
        "#D6604D",
        "#B2182B",
        "#67001F"
      ),
      limits = c(tn, tx),
      breaks = seq(tn, tx, 1),
      guide = ggplot2::guide_legend(reverse = TRUE),
      na.value = NA
    ) +
    ggplot2::labs(
      fill = "T2M (°C)",
    ) +
    ggplot2::coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 30, barheight = 1)
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text = ggplot2::element_text(size = 9),
      plot.caption = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
      legend.text = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 24),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(
        size = 20,
        margin = ggplot2::margin(r = 50)
      ),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  gg
}

# Find TN for a certain date with samba hourly product and map all products
hours_tn <- list()
for (d in seq_along(dates)) {
  idx <- which(lubridate::date(terra::time(samb)) == dates[d])
  timeserie <- terra::global(
    samb[[idx]],
    fun = c("sd", "min", "max", "mean"),
    na.rm = TRUE
  )
  timeserie$time <- terra::time(samb[[idx]])
  ts_plot <- timeserie[which(min(timeserie$mean) == timeserie$mean), c("time")]
  cat(strftime(ts_plot, tz = tz), "\n")
  hours_tn[[which(dates == dates[d])]] <- strftime(ts_plot, tz = tz)
  idx_plot_hly <- which(terra::time(samb) == ts_plot)
  idx_plot_dly <- which(terra::time(dm_tmin) == dates[d])
  tn <- floor(
    min(
      terra::minmax(samb[[idx_plot_hly]])[1],
      terra::minmax(dm_tmin[[idx_plot_dly]])[1],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[1],
      na.rm = TRUE
    )
  )
  tx <- ceiling(
    max(
      terra::minmax(samb[[idx_plot_hly]])[2],
      terra::minmax(dm_tmin[[idx_plot_dly]])[2],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[2],
      na.rm = TRUE
    )
  )
  dm_tmin <- dm_tmin |>
    terra::project("epsg:4326")
  p1 <- map_temp(samb[[idx_plot_hly]], cities, tn, tx, xlim, ylim) +
    labs(
      caption = paste0(
        "BHM T2M, min measured at\n",
        lubridate::with_tz(ts_plot, tz),
        " (local time)"
      )
    )
  p2 <- map_temp(dm_tmin[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    labs(caption = paste0("tmin daymet\n", dates[d]))
  p3 <- map_temp(gm_tmmn[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    labs(caption = paste0("tmmn gridMET\n", dates[d]))
  all <- ggpubr::ggarrange(
    p3,
    p2,
    p1,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  ggsave(
    plot = all,
    file = paste0(out_dir, "products_vs_samba_tn_", dates[d], ".png"),
    bg = "white",
    height = 5,
    width = 15,
    dpi = 300
  )
}

# other way to calculate tn
samb_tn_local <- list()
for (d in seq_along(dates)) {
  idx <- which(lubridate::date(terra::time(samb)) == dates[d])
  idx_plot_dly <- which(terra::time(dm_tmin) == dates[d])
  samb_tn_local[[d]] <- min(samb[[idx]])
  tn <- floor(
    min(
      terra::minmax(min(samb[[idx]]))[1],
      terra::minmax(dm_tmin[[idx_plot_dly]])[1],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[1],
      na.rm = TRUE
    )
  )
  tx <- ceiling(
    max(
      terra::minmax(min(samb[[idx]]))[2],
      terra::minmax(dm_tmin[[idx_plot_dly]])[2],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[2],
      na.rm = TRUE
    )
  )
  dm_tmin <- dm_tmin |>
    terra::project("epsg:4326")
  p1 <- map_temp(min(samb[[idx]]), cities, tn, tx, xlim, ylim) +
    ggplot2::labs(
      caption = paste0(
        "BHM T2M min ",
        dates[d],
        "\nmeasured at each location"
      )
    )
  p2 <- map_temp(dm_tmin[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    ggplot2::labs(caption = paste0("tmin daymet\n", dates[d]))
  p3 <- map_temp(gm_tmmn[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    ggplot2::labs(caption = paste0("tmmn gridMET\n", dates[d]))
  all <- ggpubr::ggarrange(
    p3,
    p2,
    p1,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  ggsave(
    plot = all,
    file = paste0(out_dir, "products_vs_samba_local_tn_", dates[d], ".png"),
    bg = "white",
    height = 5,
    width = 15,
    dpi = 300
  )
}

# TN avg
samb_tn <- samb[[which(
  terra::time(samb) %in% as.POSIXct(unlist(hours_tn), tz = tz)
)]]
mean_samb_tn <- terra::mean(samb_tn)
samb_tn_local <- terra::rast(samb_tn_local)
mean_samb_tn_local <- terra::mean(samb_tn_local)
mean_gm_tmmn <- terra::mean(gm_tmmn)
mean_dm_tmin <- terra::mean(dm_tmin)
tn <- floor(
  min(
    terra::minmax(mean_samb_tn_local)[1],
    terra::minmax(mean_dm_tmin)[1],
    terra::minmax(mean_gm_tmmn)[1],
    na.rm = TRUE
  )
)
tx <- ceiling(
  max(
    terra::minmax(mean_samb_tn_local)[2],
    terra::minmax(mean_dm_tmin)[2],
    terra::minmax(mean_gm_tmmn)[2],
    na.rm = TRUE
  )
)
p1 <- map_temp(
  mean_samb_tn_local,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    caption = paste0("BHM T2M, min")
  )
p2 <- map_temp(
  mean_dm_tmin,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(caption = paste0("tmin daymet\n"))
p3 <- map_temp(
  mean_gm_tmmn,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(caption = paste0("tmmn gridMET\n"))
all <- ggpubr::ggarrange(
  p3,
  p2,
  p1,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)
all
ggsave(
  plot = all,
  file = paste0(out_dir, "products_vs_samba_tn_avg.png"),
  bg = "white",
  height = 6,
  width = 18,
  dpi = 300
)


# Find tx for a certain date with samba hourly product and map all products
hours_tx <- list()
for (d in seq_along(dates)) {
  idx <- which(lubridate::date(terra::time(samb)) == dates[d])
  timeserie <- terra::global(
    samb[[idx]],
    fun = c("sd", "min", "max", "mean"),
    na.rm = TRUE
  )
  timeserie$time <- terra::time(samb[[idx]])
  ts_plot <- timeserie[which(max(timeserie$mean) == timeserie$mean), c("time")]
  cat(strftime(ts_plot, tz = tz), "\n")
  hours_tx[[which(dates == dates[d])]] <- strftime(ts_plot, tz = tz)
  idx_plot_hly <- which(terra::time(samb) == ts_plot)
  idx_plot_dly <- which(terra::time(dm_tmax) == dates[d])
  tn <- floor(
    min(
      terra::minmax(samb[[idx_plot_hly]])[1],
      terra::minmax(dm_tmax[[idx_plot_dly]])[1],
      terra::minmax(gm_tmmx[[idx_plot_dly]])[1],
      na.rm = TRUE
    )
  )
  tx <- ceiling(
    max(
      terra::minmax(samb[[idx_plot_hly]])[2],
      terra::minmax(dm_tmax[[idx_plot_dly]])[2],
      terra::minmax(gm_tmmx[[idx_plot_dly]])[2],
      na.rm = TRUE
    )
  )
  dm_tmax <- dm_tmax |>
    terra::project("epsg:4326")
  p1 <- map_temp(
    samb[[idx_plot_hly]],
    borders = cities,
    tn = tn,
    tx = tx,
    xlim = xlim,
    ylim = ylim
  ) +
    labs(
      caption = paste0(
        "BHM T2M, max measured at\n",
        lubridate::with_tz(ts_plot, tz),
        " (local time)"
      )
    )
  p2 <- map_temp(
    dm_tmax[[idx_plot_dly]],
    borders = cities,
    tn = tn,
    tx = tx,
    xlim = xlim,
    ylim = ylim
  ) +
    labs(caption = paste0("tmax daymet\n", dates[d]))
  p3 <- map_temp(
    gm_tmmx[[idx_plot_dly]],
    borders = cities,
    tn = tn,
    tx = tx,
    xlim = xlim,
    ylim = ylim
  ) +
    labs(caption = paste0("tmmx gridMET\n", dates[d]))
  all <- ggpubr::ggarrange(
    p3,
    p2,
    p1,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  ggsave(
    plot = all,
    file = paste0(out_dir, "products_vs_samba_tx_", dates[d], ".png"),
    bg = "white",
    height = 6,
    width = 18,
    dpi = 300
  )
}

# other way to calculate tx
samb_tx_local <- list()
for (d in seq_along(dates)) {
  idx <- which(lubridate::date(terra::time(samb)) == dates[d])
  idx_plot_dly <- which(terra::time(dm_tmin) == dates[d])
  samb_tx_local[[d]] <- max(samb[[idx]])
  tn <- floor(
    min(
      terra::minmax(max(samb[[idx]]))[1],
      terra::minmax(dm_tmin[[idx_plot_dly]])[1],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[1],
      na.rm = TRUE
    )
  )
  tx <- ceiling(
    max(
      terra::minmax(max(samb[[idx]]))[2],
      terra::minmax(dm_tmin[[idx_plot_dly]])[2],
      terra::minmax(gm_tmmn[[idx_plot_dly]])[2],
      na.rm = TRUE
    )
  )
  dm_tmin <- dm_tmin |>
    terra::project("epsg:4326")
  p1 <- map_temp(max(samb[[idx]]), cities, tn, tx, xlim, ylim) +
    labs(
      caption = paste0(
        "BHM T2M max ",
        dates[d],
        "\nmeasured at each location"
      )
    )
  p2 <- map_temp(dm_tmax[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    labs(caption = paste0("tmax daymet\n", dates[d]))
  p3 <- map_temp(gm_tmmx[[idx_plot_dly]], cities, tn, tx, xlim, ylim) +
    labs(caption = paste0("tmmx gridMET\n", dates[d]))
  all <- ggpubr::ggarrange(
    p3,
    p2,
    p1,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  ggsave(
    plot = all,
    file = paste0(out_dir, "products_vs_samba_local_tx_", dates[d], ".png"),
    bg = "white",
    height = 5,
    width = 15,
    dpi = 300
  )
}


# tx avg
samb_tx <- samb[[which(
  terra::time(samb) %in% as.POSIXct(unlist(hours_tx), tz = tz)
)]]
mean_samb_tx <- terra::mean(samb_tx)
samb_tx_local <- terra::rast(samb_tx_local)
mean_samb_tx_local <- terra::mean(samb_tx_local)
mean_gm_tmmx <- terra::mean(gm_tmmx)
mean_dm_tmax <- terra::mean(dm_tmax)
tn <- floor(
  min(
    terra::minmax(mean_samb_tx_local)[1],
    terra::minmax(mean_dm_tmax)[1],
    terra::minmax(mean_gm_tmmx)[1],
    na.rm = TRUE
  )
)
tx <- ceiling(
  max(
    terra::minmax(mean_samb_tx_local)[2],
    terra::minmax(mean_dm_tmax)[2],
    terra::minmax(mean_gm_tmmx)[2],
    na.rm = TRUE
  )
)
p1 <- map_temp(
  mean_samb_tx_local,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    caption = paste0("BHM T2M, max")
  )
p2 <- map_temp(
  mean_dm_tmax,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(caption = paste0("tmax daymet\n"))
p3 <- map_temp(
  mean_gm_tmmx,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(caption = paste0("tmmx gridMET\n"))
all <- ggpubr::ggarrange(
  p3,
  p2,
  p1,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)
all
ggsave(
  plot = all,
  file = paste0(out_dir, "products_vs_samba_tx_avg.png"),
  bg = "white",
  height = 6,
  width = 18,
  dpi = 300
)

devtools::load_all("../samba")

# extract daymet raster at samba locations
samb_pts_tn <- terra::as.points(mean_samb_tn_local)
samb_pts_tn$dm <- terra::extract(mean_dm_tmin, samb_pts_tn)$mean
samb_pts_tn$gm <- terra::extract(mean_gm_tmmn, samb_pts_tn)$mean
samb_pts_tn$samb <- samb_pts_tn$mean
samb_pts_tn$diff_dm <- samb_pts_tn$dm - samb_pts_tn$samb
samb_pts_tn$diff_gm <- samb_pts_tn$gm - samb_pts_tn$samb
samb_pts_tn$time <- as.POSIXct("2023-07-01 01:00:00", tz = tz)

samb_pts_tn <- sf::st_as_sf(samb_pts_tn, remove = FALSE)
samb_pts_tn$lon <- sf::st_coordinates(samb_pts_tn)[, 1]
samb_pts_tn$lat <- sf::st_coordinates(samb_pts_tn)[, 2]

diff_dm_tn <- samb_pts_tn |>
  sf::st_as_sf(remove = FALSE) |>
  rasterize_pred(varname = "diff_dm")
diff_gm_tn <- samb_pts_tn |>
  sf::st_as_sf(remove = FALSE) |>
  rasterize_pred(varname = "diff_gm")

samb_pts_tx <- terra::as.points(mean_samb_tx_local)
samb_pts_tx$dm <- terra::extract(mean_dm_tmax, samb_pts_tx)$mean
samb_pts_tx$gm <- terra::extract(mean_gm_tmmx, samb_pts_tx)$mean
samb_pts_tx$samb <- samb_pts_tx$mean
samb_pts_tx$diff_dm <- samb_pts_tx$dm - samb_pts_tx$samb
samb_pts_tx$diff_gm <- samb_pts_tx$gm - samb_pts_tx$samb
samb_pts_tx$time <- as.POSIXct("2023-07-01 01:00:00", tz = tz)

samb_pts_tx <- sf::st_as_sf(samb_pts_tx, remove = FALSE)
samb_pts_tx$lon <- sf::st_coordinates(samb_pts_tx)[, 1]
samb_pts_tx$lat <- sf::st_coordinates(samb_pts_tx)[, 2]

diff_dm_tx <- samb_pts_tx |>
  sf::st_as_sf(remove = FALSE) |>
  rasterize_pred(varname = "diff_dm")
diff_gm_tx <- samb_pts_tx |>
  sf::st_as_sf(remove = FALSE) |>
  rasterize_pred(varname = "diff_gm")

tn <- floor(
  min(
    terra::minmax(diff_dm_tn)[1],
    terra::minmax(diff_gm_tn)[1],
    terra::minmax(diff_dm_tx)[1],
    terra::minmax(diff_gm_tx)[1],
    na.rm = TRUE
  )
)
tx <- ceiling(
  max(
    terra::minmax(diff_dm_tn)[2],
    terra::minmax(diff_gm_tn)[2],
    terra::minmax(diff_dm_tx)[2],
    terra::minmax(diff_gm_tx)[2],
    na.rm = TRUE
  )
)

p_diff_dm_tn <- map_temp(
  diff_dm_tn,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    fill = latex2exp::TeX("$\\Delta$ T2M (°C)"),
    caption = "TMIN daymet - BHM T2M min"
  ) +
  scale_fill_stepsn(
    colours = c("blue", "#F7F7F7", "#D6604D"),
    na.value = NA,
    limits = c(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx))),
    breaks = seq(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx)), 1),
    guide = guide_legend(reverse = TRUE)
  )

p_diff_gm_tn <- map_temp(
  diff_gm_tn,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    fill = latex2exp::TeX("$\\Delta$ T2M (°C)"),
    caption = "TMMN gridMET - BHM T2M min"
  ) +
  scale_fill_stepsn(
    colours = c("blue", "#F7F7F7", "#D6604D"),
    na.value = NA,
    limits = c(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx))),
    breaks = seq(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx)), 1),
    guide = guide_legend(reverse = TRUE)
  )
all_tn_diff <- ggpubr::ggarrange(
  p_diff_dm_tn,
  p_diff_gm_tn,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)
all_tn_diff
ggsave(
  plot = all_tn_diff,
  file = paste0(out_dir, "products_vs_samba_tn_local_avg_diff.png"),
  bg = "white",
  height = 6,
  width = 12,
  dpi = 300
)

p_diff_dm_tx <- map_temp(
  diff_dm_tx,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    fill = latex2exp::TeX("$\\Delta$ T2M (°C)"),
    caption = "TMAX daymet - BHM T2M max"
  ) +
  scale_fill_stepsn(
    colours = c("blue", "#F7F7F7", "#D6604D"),
    na.value = NA,
    limits = c(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx))),
    breaks = seq(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx)), 1),
    guide = guide_legend(reverse = TRUE)
  )

p_diff_gm_tx <- map_temp(
  diff_gm_tx,
  borders = cities,
  tn = tn,
  tx = tx,
  xlim = xlim,
  ylim = ylim
) +
  labs(
    fill = latex2exp::TeX("$\\Delta$ T2M (°C)"),
    caption = "TMMX gridMET - BHM T2M max"
  ) +
  scale_fill_stepsn(
    colours = c("blue", "#F7F7F7", "#D6604D"),
    na.value = NA,
    limits = c(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx))),
    breaks = seq(-max(abs(tn), abs(tx)), max(abs(tn), abs(tx)), 1),
    guide = guide_legend(reverse = TRUE)
  )
all_tx_diff <- ggpubr::ggarrange(
  p_diff_dm_tx,
  p_diff_gm_tx,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)
all_tx_diff
ggsave(
  plot = all_tx_diff,
  file = paste0(out_dir, "products_vs_samba_tx_local_avg_diff.png"),
  bg = "white",
  height = 6,
  width = 12,
  dpi = 300
)
