#' Plot price index output
#'
#' Static price index plot using base R graphics with grid lines and external legend.
#'
#' Supports output from `calculate_hedonic_index()` and plot-ready output from
#' `calculate_spar()`, as either a single data frame or named list of methods.
#' Each data frame must contain `period` and `Index`, with one row per period.
#' Grouped SPAR output should be filtered to one series before plotting.
#' X-axis shows only first period of each year with rotated labels to avoid clutter.
#'
#' @author Vivek Gajadhar
#' @param index_output A data frame or named list of data frames containing
#'   `period` and `Index` columns.
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

  validate_plot_series <- function(data, label = "index_output") {
    if (!is.data.frame(data) ||
        !("period" %in% names(data)) ||
        !("Index" %in% names(data))) {
      stop(
        "`", label, "` must be a data frame with `period` and `Index` columns.",
        call. = FALSE
      )
    }
    if (anyDuplicated(data$period)) {
      stop(
        "`", label, "` contains multiple rows for the same period. ",
        "Filter grouped output to one series before plotting.",
        call. = FALSE
      )
    }
    invisible(TRUE)
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
    has_method_names <- length(index_output) > 0L &&
      !is.null(names(index_output)) &&
      all(nzchar(names(index_output)))
    valid_series <- vapply(index_output, function(x) {
      is.data.frame(x) && "period" %in% names(x) && "Index" %in% names(x)
    }, logical(1))
    is_valid_multi <- has_method_names && all(valid_series)

    if (!is_valid_multi) {
      invalid_entries <- if (length(valid_series) > 0L) {
        labels <- names(index_output)
        if (is.null(labels)) {
          labels <- paste0("[[", seq_along(index_output), "]]")
        } else {
          labels[!nzchar(labels)] <- paste0(
            "[[", which(!nzchar(labels)), "]]"
          )
        }
        labels[!valid_series]
      } else {
        character(0)
      }
      details <- if (length(invalid_entries) > 0L) {
        paste0(" Invalid entries: ", paste(invalid_entries, collapse = ", "), ".")
      } else {
        ""
      }
      stop(
        "The input list is not a valid multi-method output. ",
        "It must be a non-empty named list in which every element is a data ",
        "frame containing `period` and `Index` columns.",
        details,
        call. = FALSE
      )
    }

    for (method_name in names(index_output)) {
      validate_plot_series(
        index_output[[method_name]],
        paste0("index_output$", method_name)
      )
    }

    combined <- do.call(rbind, lapply(names(index_output), function(name) {
      df <- index_output[[name]]
      df <- df[order(df$period), ]
      data.frame(
        period = df$period,
        Index = df$Index,
        method = name,
        stringsAsFactors = FALSE
      )
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
    method_colors <- cb_palette[(seq_along(methods) - 1) %% length(cb_palette) + 1]
    method_line_types <- rep(c(1, 2, 3, 4, 5, 6), length.out = length(methods))
    method_point_symbols <- rep(
      c(19, 1, 17, 2, 15, 0, 18, 5),
      length.out = length(methods)
    )
    for (i in seq_along(methods)) {
      method_name <- methods[i]
      df <- combined[combined$method == method_name, ]
      df <- df[order(df$period), ]
      lines(
        x,
        df$Index,
        type = "b",
        pch = method_point_symbols[i],
        lty = method_line_types[i],
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
      pch = method_point_symbols,
      lty = method_line_types,
      bty = "n",
      xpd = TRUE
    )
  } else {
    stop(
      "Unsupported input type: supply a data frame or named list of data ",
      "frames returned by `calculate_hedonic_index()` or `calculate_spar()`.",
      call. = FALSE
    )
  }
}
