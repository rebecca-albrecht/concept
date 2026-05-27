#' Plot mean concept change effect by group
#'
#' Plots the mean of a selected effect measure by one grouping variable,
#' optionally faceted by a second grouping variable. Error bars show
#' approximate 95% confidence intervals based on the standard error.
#'
#' @param data A data frame containing the effect estimates.
#' @param measure Name of the numeric effect variable to plot.
#' @param group1 Name of the primary grouping variable shown on the x-axis.
#' @param group2 Optional name of a second grouping variable used for faceting.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
plot_effect_group <- function(data, measure, group1, group2 = NULL) {
  measure_sym <- rlang::sym(measure)
  group1_sym <- rlang::sym(group1)

  if (!is.null(group2)) {
    group2_sym <- rlang::sym(group2)

    data_p <- data |>
      dplyr::group_by(!!group1_sym, !!group2_sym)
  } else {
    data_p <- data |>
      dplyr::group_by(!!group1_sym)
  }

  data_sum <- data_p |>
    dplyr::summarise(
      change_mean = mean(!!measure_sym, na.rm = TRUE),
      change_se = stats::sd(!!measure_sym, na.rm = TRUE) /
        sqrt(sum(!is.na(!!measure_sym))),
      .groups = "drop"
    )

  pl <- data_sum |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!group1_sym,
        y = .data$change_mean,
        fill = !!group1_sym
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$change_mean - 1.96 * .data$change_se,
        ymax = .data$change_mean + 1.96 * .data$change_se
      ),
      width = 0.1
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::labs(
      x = group1,
      y = "Mean concept change effect",
      fill = group1
    )

  if (!is.null(group2)) {
    pl <- pl + ggplot2::facet_wrap(stats::as.formula(paste("~", group2)))
  }

  pl
}

#' Save a plot
#'
#' Internal helper for saving ggplot objects.
#'
#' @param plot A ggplot object.
#' @param filename Output file path, including file extension.
#' @param height Plot height in inches.
#' @param width Plot width in inches.
#'
#' @return Invisibly returns the input filename.
#'
#' @keywords internal
.save_plot <- function(plot, filename, height = 8, width = 8) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    height = height,
    width = width
  )

  invisible(filename)
}
