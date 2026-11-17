#' Plot price index output
#'
#' Static price index plot using base R graphics with grid lines and external legend.
#'
#' Supports single index data.frames and named lists of methods from
#' `calculate_hedonic_index()` or `calculate_spar_index()`. Each frame must have one
#' row per period. Filter grouped SPAR output to one series before plotting.
#' Optional bootstrap margins are drawn as translucent confidence bands.
#' X-axis shows only first period of each year with rotated labels to avoid clutter.
#'
#' @author Vivek Gajadhar
#' @param index_output A data.frame or named list of data.frames containing
#'   `period` and `Index` columns; optionally `lower_margin` and `upper_margin`.
#' @param title Optional plot title
#' @return None. Draws plots in the active graphics device.
#' @importFrom graphics axis grid legend lines par plot polygon text
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

  validate_plot_series <- function(data) {
    if (!is.data.frame(data) || !all(c("period", "Index") %in% names(data))) {
      stop("Each plot series must be a data.frame with 'period' and 'Index' columns.")
    }
    if (anyDuplicated(data$period)) {
      stop("The output contains multiple rows for the same period; filter grouped output to one series before plotting.")
    }
    if (!nrow(data) || anyNA(data$period) || !is.numeric(data$Index) ||
        !any(is.finite(data$Index))) {
      stop("Each plot series must contain periods and finite numeric index values.")
    }
    if (all(c("lower_margin", "upper_margin") %in% names(data)) &&
        (!is.numeric(data$lower_margin) || !is.numeric(data$upper_margin))) {
      stop("Bootstrap margin columns must be numeric.")
    }
  }

  valid_margins <- function(data) {
    if (!all(c("lower_margin", "upper_margin") %in% names(data))) {
      return(rep(FALSE, nrow(data)))
    }
    is.finite(data$lower_margin) & is.finite(data$upper_margin) &
      data$lower_margin <= data$upper_margin
  }

  range_with_margins <- function(data) {
    good <- valid_margins(data)
    values <- data$Index
    if (any(good)) {
      values <- c(values, data$lower_margin[good], data$upper_margin[good])
    }
    range(values[is.finite(values)])
  }

  draw_margins <- function(x, data, color) {
    good <- which(valid_margins(data))
    if (length(good) < 2L) return(invisible(NULL))
    segments <- split(good, cumsum(c(1L, as.integer(diff(good) != 1L))))
    for (rows in segments) {
      if (length(rows) < 2L) next
      graphics::polygon(
        c(x[rows], rev(x[rows])),
        c(data$lower_margin[rows], rev(data$upper_margin[rows])),
        col = grDevices::adjustcolor(color, alpha.f = 0.2),
        border = NA
      )
    }
    invisible(NULL)
  }

  if (is.null(title)) {
    if (is.data.frame(index_output)) {
      title <- "Price Index"
    } else if (is.list(index_output)) {
      title <- "Price Index Comparison"
    }
  }

  if (is.data.frame(index_output)) {
    validate_plot_series(index_output)
    df <- index_output[order(index_output$period), ]
    periods <- as.factor(df$period)
    period_levels <- levels(periods)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)

    plot(
      x, df$Index,
      type = "n",
      ylim = range_with_margins(df),
      xaxt = "n",
      xlab = "",
      ylab = "Index",
      main = title
    )
    grid(col = "grey90", lty = "dotted")
    draw_margins(x, df, cb_palette[1])
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
    is_valid_multi <- length(index_output) > 0L &&
      !is.null(names(index_output)) && !anyNA(names(index_output)) &&
      all(nzchar(names(index_output))) && !anyDuplicated(names(index_output)) &&
      all(vapply(index_output, function(x) {
      is.data.frame(x) && "period" %in% names(x) && "Index" %in% names(x)
    }, logical(1)))

    if (!is_valid_multi) {
      stop(paste(
        "Error: The input list is not a valid multi-method output.",
        "This usually happens if you used 'resting_points = TRUE' with HMTS.",
        "Please pass the specific index dataframe to the plot function instead.",
        "Example: plot_price_index(result$Index)"
      ))
    }

    invisible(lapply(index_output, validate_plot_series))

    combined <- do.call(rbind, lapply(names(index_output), function(name) {
      df <- index_output[[name]]
      df <- df[order(df$period), ]
      data.frame(period = as.character(df$period), Index = df$Index, method = name)
    }))
    combined$period <- as.factor(combined$period)
    period_levels <- levels(combined$period)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)
    y_range <- range(unlist(lapply(index_output, range_with_margins)))

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
    method_colors <- cb_palette[(seq_along(methods) - 1) %% length(cb_palette) + 1]
    for (i in seq_along(methods)) {
      method_name <- methods[i]
      df <- index_output[[method_name]]
      df <- df[order(df$period), ]
      coordinates <- match(as.character(df$period), period_levels)
      draw_margins(coordinates, df, method_colors[i])
      lines(
        coordinates,
        df$Index,
        type = "b",
        pch = 19,
        col = method_colors[i]
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
      col = method_colors,
      pch = 19,
      lty = 1,
      bty = "n",
      xpd = TRUE
    )
  } else {
    stop("Unsupported input type: supply a data.frame or named list of data.frames from calculate_hedonic_index() or calculate_spar_index().")
  }
}
