# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide_setup <- function(data, width = NULL, name, strategy,
                          check.width = TRUE, reverse = FALSE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }

    # Width determined from data, must be floating point constant
    widths <- unique(data$xmax - data$xmin)
    widths <- widths[!is.na(widths)]

#   # Suppress warning message since it's not reliable
#     if (!zero_range(range(widths))) {
#       warning(name, " requires constant width: output may be incorrect",
#         call. = FALSE)
#     }
    width <- widths[1]
  }

  list(data = data, width = width)
}

collide <- function(data, width = NULL, name, strategy,
                    ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$xmin, data$group), ]
  } else {
    data <- data[order(data$xmin, -data$group), ]
  }

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$ymax)) {
    dapply(data, "xmin", strategy, ..., width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- dapply(data, "xmin", strategy, ..., width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}

# Alternate version of collide() used by position_dodge2()
collide2 <- function(data, width = NULL, name, strategy,
                     ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order is
  # different than for collide() because of the order in which pos_dodge2 places
  # elements
  if (reverse) {
    data <- data[order(data$x, -data$group), ]
  } else {
    data <- data[order(data$x, data$group), ]
  }

  pos <- match.fun(strategy)
  pos(data, width, ...)
}
