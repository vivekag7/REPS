#' Plot index output from calculate_hedonic_index
#'
#' Static price index plot using base R graphics with grid lines and external legend.
#'
#' Supports both single index data.frame and named list of multiple methods.
#' X-axis shows only first period of each year with rotated labels to avoid clutter.
#'
#' @author Vivek Gajadhar
#' @param index_output A data.frame or named list of data.frames (from calculate_hedonic_index())
#' @param title Optional plot title
#' @return None. Draws plots in the active graphics device.
#' @importFrom graphics axis grid legend lines par plot text
#' @export
plot_price_index <- function(index_output, title = NULL) {

  op <- par(mfrow = c(1, 1))
  on.exit(par(op))

  get_year_start_periods <- function(periods) {
    years <- substr(periods, 1, 4)
    periods[!duplicated(years)]
  }

  cb_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )

  if (is.null(title)) {
    if (is.data.frame(index_output)) {
      title <- "Price Index"
    } else if (is.list(index_output)) {
      title <- "Price Index Comparison"
    }
  }

  if (is.data.frame(index_output)) {
    df <- index_output[order(index_output$period), ]
    periods <- as.factor(df$period)
    period_levels <- levels(periods)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)

    plot(
      x, df$Index,
      type = "n",
      xaxt = "n",
      xlab = "",
      ylab = "Index",
      main = title
    )
    grid(col = "grey90", lty = "dotted")
    lines(x, df$Index, type = "b", pch = 19, col = cb_palette[1])
    axis(1, at = break_indices, labels = FALSE)
    text(
      x = break_indices,
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
      labels = breaks,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      cex = 0.8
    )
  } else if (is.list(index_output)) {
    is_valid_multi <- all(sapply(index_output, function(x) {
      is.data.frame(x) && "period" %in% names(x) && "Index" %in% names(x)
    }))

    if (!is_valid_multi) {
      stop(paste(
        "Error: The input list is not a valid multi-method output.",
        "This usually happens if you used 'resting_points = TRUE' with HMTS.",
        "Please pass the specific index dataframe to the plot function instead.",
        "Example: plot_price_index(result$Index)"
      ))
    }

    combined <- do.call(rbind, lapply(names(index_output), function(name) {
      df <- index_output[[name]]
      df <- df[order(df$period), ]
      df$method <- name
      df
    }))
    combined$period <- as.factor(combined$period)
    period_levels <- levels(combined$period)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)
    y_range <- range(combined$Index, na.rm = TRUE)

    plot(
      NA,
      xlim = range(x),
      ylim = y_range,
      xaxt = "n",
      xlab = "",
      ylab = "Index",
      main = title
    )
    grid(col = "grey90", lty = "dotted")

    methods <- unique(combined$method)
    for (i in seq_along(methods)) {
      method_name <- methods[i]
      df <- combined[combined$method == method_name, ]
      df <- df[order(df$period), ]
      lines(
        x,
        df$Index,
        type = "b",
        pch = 19,
        col = cb_palette[(i - 1) %% length(cb_palette) + 1]
      )
    }

    axis(1, at = break_indices, labels = FALSE)
    text(
      x = break_indices,
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
      labels = breaks,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      cex = 0.8
    )

    legend(
      "bottomright",
      legend = methods,
      col = cb_palette[seq_along(methods)],
      pch = 19,
      lty = 1,
      bty = "n",
      xpd = TRUE
    )
  } else {
    stop("Unsupported input type: must be a data.frame or named list of data.frames from calculate_hedonic_index()")
  }
}
