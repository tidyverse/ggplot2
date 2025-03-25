bins <- function(breaks, closed = "right",
                 fuzz = NULL) {
  check_numeric(breaks)
  closed <- arg_match0(closed, c("right", "left"))
  breaks <- sort(breaks)

  # Adapted base::hist - this protects from floating point rounding errors
  fuzz <- fuzz %||% 1e-08 * stats::median(diff(breaks[is.finite(breaks)]))
  if (!is.finite(fuzz)) { # happens when 0 or 1 finite breaks are given
    fuzz <- .Machine$double.eps * 1e3
  }
  if (closed == "right") {
    fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
  }

  structure(
    list(
      breaks = breaks,
      fuzzy = breaks + fuzzes,
      right_closed = closed == "right"
    ),
    class = "ggplot2_bins"
  )
}

is_bins <- function(x) inherits(x, "ggplot2_bins")

#' @export
print.ggplot2_bins <- function(x, ...) {
  n <- length(x$breaks)
  cat("<Bins>\n")

  if (x$right_closed) {
    left <- c("[", rep("(", n - 2))
    right <- rep("]", n - 1)
  } else {
    left <- rep("[", n - 1)
    right <- c(rep(")", n - 2), "]")
  }

  breaks <- format(x$breaks)
  bins <- paste0("* ", left, breaks[-n], ",", breaks[-1], right)
  cat(bins, sep = "\n")
  cat("\n")
}

# Compute parameters -----------------------------------------------------------

bin_breaks <- function(breaks, closed = c("right", "left")) {
  bins(breaks, closed)
}

bin_breaks_width <- function(x_range, width = NULL, center = NULL,
                             boundary = NULL, closed = c("right", "left")) {

  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  # Find the left side of left-most bin: inputs could be Dates or POSIXct, so
  # coerce to numeric first.
  shift <- floor((x_range[1] - boundary) / width)
  origin <- boundary + shift * width

  # Small correction factor so that we don't get an extra bin when, for
  # example, origin = 0, max(x) = 20, width = 10.
  max_x <- x_range[2] + (1 - 1e-08) * width

  if (isTRUE((max_x - origin) / width > 1e6)) {
    cli::cli_abort(c(
      "The number of histogram bins must be less than 1,000,000.",
      "i" = "Did you make {.arg binwidth} too small?"
    ))
  }
  breaks <- seq(origin, max_x, width)

  if (length(breaks) == 1) {
    # In exceptionally rare cases, the above can fail and produce only a
    # single break (see issue #3606). We fix this by adding a second break.
    breaks <- c(breaks, breaks + width)
  }

  bin_breaks(breaks, closed = closed)
}

bin_breaks_bins <- function(x_range, bins = 30, center = NULL,
                            boundary = NULL, closed = c("right", "left")) {

  if (zero_range(x_range)) {
    # 0.1 is the same width as the expansion `default_expansion()` gives for 0-width data
    width <- 0.1
  } else if (bins == 1) {
    width <- diff(x_range)
    boundary <- x_range[1]
    center <- NULL
  } else {
    width <- (x_range[2] - x_range[1]) / (bins - 1)
    if (is.null(center)) {
      boundary <- boundary %||% x_range[1] - width / 2
    }
  }

  bin_breaks_width(x_range, width, boundary = boundary, center = center,
    closed = closed)
}


# Compute bins ------------------------------------------------------------

compute_bins <- function(x, scale = NULL, breaks = NULL, binwidth = NULL, bins = NULL,
                         center = NULL, boundary = NULL,
                         closed = c("right", "left")) {

  range <- if (is.scale(scale)) scale$dimension() else range(x)
  check_length(range, 2L)

  if (!is.null(breaks)) {
    breaks <- allow_lambda(breaks)
    if (is.function(breaks)) {
      breaks <- breaks(x)
    }
    if (is.scale(scale) && !scale$is_discrete()) {
      breaks <- scale$transform(breaks)
    }
    check_numeric(breaks)
    bins <- bin_breaks(breaks, closed)
    return(bins)
  }

  check_number_decimal(boundary, allow_infinite = FALSE, allow_null = TRUE)
  check_number_decimal(center, allow_infinite = FALSE, allow_null = TRUE)
  if (!is.null(boundary) && !is.null(center)) {
    cli::cli_abort("Only one of {.arg boundary} and {.arg center} may be specified.")
  }

  if (!is.null(binwidth)) {
    binwidth <- allow_lambda(binwidth)
    if (is.function(binwidth)) {
      binwidth <- binwidth(x)
    }
    check_number_decimal(binwidth, min = 0, allow_infinite = FALSE)
    bins <- bin_breaks_width(
      range, binwidth,
      center = center, boundary = boundary, closed = closed
    )
    return(bins)
  }

  bins <- allow_lambda(bins)
  if (is.function(bins)) {
    bins <- bins(x)
  }
  check_number_whole(bins, min = 1, allow_infinite = FALSE)
  bin_breaks_bins(
    range, bins,
    center = center, boundary = boundary, closed = closed
  )
}

bin_vector <- function(x, bins, weight = NULL, pad = FALSE) {
  check_object(bins, is_bins, "a {.cls ggplot2_bins} object")

  if (all(is.na(x))) {
    return(bin_out(length(x), NA, NA, xmin = NA, xmax = NA))
  }

  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }

  bin_idx <- bin_cut(x, bins)
  bin_count <- as.numeric(tapply(weight, bin_idx, sum, na.rm = TRUE))
  bin_count[is.na(bin_count)] <- 0

  bin_x <- (bins$breaks[-length(bins$breaks)] + bins$breaks[-1]) / 2
  bin_widths <- diff(bins$breaks)

  # Pad row of 0s at start and end
  if (pad) {
    bin_count <- c(0, bin_count, 0)

    width1 <- bin_widths[1]
    widthn <- bin_widths[length(bin_widths)]

    bin_widths <- c(width1, bin_widths, widthn)
    bin_x <- c(bin_x[1] - width1, bin_x, bin_x[length(bin_x)] + widthn)
  }

  # Add row for missings
  if (anyNA(bins)) {
    bin_count <- c(bin_count, sum(is.na(bins)))
    bin_widths <- c(bin_widths, NA)
    bin_x <- c(bin_x, NA)
  }

  bin_out(bin_count, bin_x, bin_widths)
}

bin_cut <- function(x, bins) {
  cut(x, bins$fuzzy, right = bins$right_closed, include.lowest = TRUE)
}

bin_out <- function(count = integer(0), x = numeric(0), width = numeric(0),
  xmin = x - width / 2, xmax = x + width / 2) {
  density <- count / width / sum(abs(count))

  data_frame0(
    count = count,
    x = x,
    xmin = xmin,
    xmax = xmax,
    width = width,
    density = density,
    ncount = count / max(abs(count)),
    ndensity = density / max(abs(density)),
    .size = length(count)
  )
}

bin_loc <- function(x, id) {
  left <- x[-length(x)]
  right <- x[-1]

  list(
    left = left[id],
    right = right[id],
    mid = ((left + right) / 2)[id],
    length = diff(x)[id]
  )
}

fix_bin_params <- function(params, fun, version) {

  if (package_version(version) < "3.0.0") {
    deprecate <- lifecycle::deprecate_stop
  } else {
    deprecate <- deprecate_warn0
  }

  if (!is.null(params$origin)) {
    args <- paste0(fun, c("(origin)", "(boundary)"))
    deprecate(version, args[1], args[2])
    params$boundary <- params$origin
    params$origin <- NULL
  }

  if (!is.null(params$right)) {
    args <- paste0(fun, c("(right)", "(closed)"))
    deprecate(version, args[1], args[2])
    params$closed <- if (isTRUE(params$right)) "right" else "left"
    params$right <- NULL
  }

  if (is.null(params$breaks %||% params$binwidth %||% params$bins)) {
    cli::cli_inform(
      "{.fn {fun}} using {.code bins = 30}. Pick better value {.arg binwidth}."
    )
    params$bins <- 30
  }

  params
}
