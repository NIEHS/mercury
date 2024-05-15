#' @title scatterplot_deltat_vs_imp
#' @description Plot temperature difference to HeatWatch in fonction of
#' imperviousness
#' @param diff_df_am data.frame. Data frame with the temperature difference to
#' heatwatch in the morning (6-7am)
#' @param diff_df_af data.frame. Data frame with the temperature difference to
#' heatwatch in the afternoon (3-4pm)
#' @return ggplot objects
#' @import latex2exp
#' @import ggplot2
#' @export
scatterplot_deltat_vs_imp <- function(diff_df_am, diff_df_af) {
  p_dm_am <- ggplot(diff_df_am[which(diff_df_am$imp < 100), ],
         aes(x = imp, y = delta_daymet)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.01) +
    geom_smooth(method = "lm") +
    xlab("Imperviousness") +
    ylab(latex2exp::TeX("$TN_{dm}-T_{am\\_hw}$ (°C)")) +
    labs(caption = expression(
      italic('Data sources: Heatwatch campaign and Daymet'))) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_gm_am <- ggplot(diff_df_am[which(diff_df_am$imp < 100), ],
                 aes(x = imp, y = delta_gridmet)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.01) +
    geom_smooth(method = "lm") +
    xlab("Imperviousness") +
    ylab(latex2exp::TeX("$TN_{gm}-T_{am\\_hw}$ (°C)")) +
    labs(caption = expression(
      italic('Data sources: Heatwatch campaign and gridMET'))) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_dm_af <- ggplot(diff_df_af[which(diff_df_af$imp < 100), ],
                    aes(x = imp, y = delta_daymet)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.01) +
    geom_smooth(method = "lm") +
    xlab("Imperviousness") +
    ylab(latex2exp::TeX("$TX_{dm}-T_{af\\_hw}$ (°C)")) +
    labs(caption = expression(
      italic('Data sources: Heatwatch campaign and Daymet'))) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_gm_af <- ggplot(diff_df_af[which(diff_df_af$imp < 100), ],
                    aes(x = imp, y = delta_gridmet)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
    geom_point(alpha = 0.01) +
    geom_smooth(method = "lm") +
    xlab("Imperviousness") +
    ylab(latex2exp::TeX("$TX_{gm}-T_{af\\_hw}$ (°C)")) +
    labs(caption = expression(
      italic('Data sources: Heatwatch campaign and gridMET'))) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  return(list("p_dm_am" = p_dm_am,
              "p_gm_am" = p_gm_am,
              "p_dm_af" = p_dm_af,
              "p_gm_af" = p_gm_af))

}


#' @title boxplot_deltat_vs_nlcd
#' @description Boxplot temperature difference to HeatWatch in fonction of NLCD
#' @param diff_df_am data.frame. Data frame with the temperature difference to
#' heatwatch in the morning (6-7am)
#' @param diff_df_af data.frame. Data frame with the temperature difference to
#' heatwatch in the afternoon (3-4pm)
#' @return ggplot objects
#' @import latex2exp
#' @import ggplot2
#' @export
boxplot_deltat_vs_nlcd <- function(diff_df_am, diff_df_af) {
  # nlcd classes dataframe
  nlcd_classes <- list(
    value = c(
      11, 21, 22, 23, 24, 31, 41, 42, 43, 52,
      71, 81, 82, 90, 95
    ),
    class = c(
      "WTR", "OSD", "LID", "MID", "HID",
      "BRN", "DFO", "EFO", "MFO", "SHB",
      "GRS", "PAS", "CRP", "WDW", "EHW"
    ),
    names = c(
      "Open Water",
      "Developed, Open Space",
      "Developed, Low Intensity",
      "Developed, Medium Intensity",
      "Developed, High Intensity",
      "Barren Land",
      "Deciduous Forest",
      "Evergreen Forest",
      "Mixed Forest",
      "Shrub/Scrub",
      "Herbaceous",
      "Hay/Pasture",
      "Cultivated Crops",
      "Woody Wetlands",
      "Emergent Herbaceous Wetlands"
    ),
    col = c(
      "#476ba1", "#decaca", "#d99482", "#ee0000",
      "#ab0000", "#b3aea3", "#68ab63", "#1c6330",
      "#b5ca8f", "#ccba7d", "#e3e3c2", "#dcd93d",
      "#ab7028", "#bad9eb", "#70a3ba"
    )
  )
  nlcd_classes <- as.data.frame(nlcd_classes)
  val <- nlcd_classes$col
  names(val) <- as.factor(nlcd_classes$value)
  lab <- nlcd_classes$names
  names(lab) <- as.factor(nlcd_classes$value)

  # bpxplots
  p_dm_am <- ggplot(diff_df_am,
         aes(x = as.factor(nlcd),
             y = delta_daymet,
             group = as.factor(nlcd),
             fill = as.factor(nlcd))) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "blue") +
    geom_boxplot() +
    xlab("NLCD") +
    ylab(latex2exp::TeX("$TN_{dm}-T_{am\\_hw}$ (°C)")) +
    labs(
      caption = expression(
        italic('Data sources: Heatwatch campaign and Daymet')),
      fill = "NLCD classes") +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    scale_fill_manual(values = val, labels = lab) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_gm_am <- ggplot(diff_df_am,
                    aes(x = as.factor(nlcd),
                        y = delta_gridmet,
                        group = as.factor(nlcd),
                        fill = as.factor(nlcd))) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "blue") +
    geom_boxplot() +
    xlab("NLCD") +
    ylab(latex2exp::TeX("$TN_{gm}-T_{am\\_hw}$ (°C)")) +
    labs(
      caption = expression(
        italic('Data sources: Heatwatch campaign and gridMET')),
      fill = "NLCD classes") +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    scale_fill_manual(values = val, labels = lab) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_dm_af <- ggplot(diff_df_af,
                    aes(x = as.factor(nlcd),
                        y = delta_daymet,
                        group = as.factor(nlcd),
                        fill = as.factor(nlcd))) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "blue") +
    geom_boxplot() +
    xlab("NLCD") +
    ylab(latex2exp::TeX("$TX_{dm}-T_{af\\_hw}$ (°C)")) +
    labs(
      caption = expression(
        italic('Data sources: Heatwatch campaign and Daymet')),
      fill = "NLCD classes") +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    scale_fill_manual(values = val, labels = lab) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )

  p_gm_af <- ggplot(diff_df_af,
                    aes(x = as.factor(nlcd),
                        y = delta_gridmet,
                        group = as.factor(nlcd),
                        fill = as.factor(nlcd))) +
    geom_hline(yintercept = 0, color = "red") +
    geom_hline(yintercept = -2, color = "blue") +
    geom_boxplot() +
    xlab("NLCD") +
    ylab(latex2exp::TeX("$TX_{gm}-T_{af\\_hw}$ (°C)")) +
    labs(
      caption = expression(
        italic('Data sources: Heatwatch campaign and gridMET')),
      fill = "NLCD classes") +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-10, 5)) +
    scale_fill_manual(values = val, labels = lab) +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(list("p_dm_am" = p_dm_am,
              "p_gm_am" = p_gm_am,
              "p_dm_af" = p_dm_af,
              "p_gm_af" = p_gm_af))
}

