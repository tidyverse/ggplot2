# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide <- function(data, width = NULL, name, strategy, check.width = TRUE) {
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

  # Reorder by x position, relying on stable sort to preserve existing
  # ordering, which may be by group or order.
  data <- data[order(data$xmin), ]

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$ymax)) {
    plyr::ddply(data, "xmin", strategy, width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- plyr::ddply(data, "xmin", strategy, width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}

# Stack overlapping intervals.
# Assumes that each set has the same horizontal position
pos_stack <- function(df, width) {
  if (nrow(df) == 1) return(df)

  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  if (all(is.na(df$x))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(y))
  }

  df$ymin <- heights[-n]
  df$ymax <- heights[-1]
  df$y <- df$ymax
  df
}

# Stack overlapping intervals and set height to 1.
# Assumes that each set has the same horizontal position.
pos_fill <- function(df, width) {
  stacked <- pos_stack(df, width)
  stacked$ymin <- stacked$ymin / max(stacked$ymax)
  stacked$ymax <- stacked$ymax / max(stacked$ymax)
  stacked$y <- stacked$ymax
  stacked
}

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodge <- function(df, width) {
  n <- length(unique(df$group))
  if (n == 1) return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)

  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # ggplot(df, aes(n, div)) + geom_point()

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}
