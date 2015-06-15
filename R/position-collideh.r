# Functions for horizontal direction

collideh <- function(data, height = NULL, name, strategy, check.height = TRUE) {

  # Determine height
  if (!is.null(height)) {
    # Width set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y - height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }

    # Height determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]
    if (!zero_range(range(heights))) {
      warning(name, " requires constant height: output may be incorrect",
        call. = FALSE)
    }
    height <- heights[1]
  }

  # Reorder by y position, relying on stable sort to preserve existing
  # ordering, which may be by group or order.
  data <- data[order(data$ymin), ]

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$xmax)) {
    ddply(data, "ymin", strategy, height = height)
  } else if (!is.null(data$x)) {
    message("xmax not defined: adjusting position using x instead")

    data$xmax <- data$x
    data <- ddply(data, "ymin", strategy, height = height)
    data$x <- data$xmax
    data
  } else {
    stop("Neither x nor xmax defined")
  }
}

# Stack overlapping intervals.
# Assumes that each set has the same horizontal position
pos_stackh <- function(df, height) {
  if (nrow(df) == 1) return(df)

  n <- nrow(df) + 1
  x <- with(df, ifelse(is.na(x), 0, x))
  if (all(is.na(df$y))) {
    widths <- rep(NA, n)
  } else {
    widths <- c(0, cumsum(x))
  }

  within(df, {
    xmin <- widths[-n]
    xmax <- widths[-1]
    x <- xmax
  })
}

# Stack overlapping intervals and set width to 1.
# Assumes that each set has the same vertical position.
pos_fillh <- function(df, height) {
  within(pos_stackh(df, height), {
    xmin <- xmin / max(xmax)
    xmax <- xmax / max(xmax)
    x <- xmax
  })
}

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodgeh <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) return(df)

  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }

  d_height <- max(df$ymax - df$ymin)
  diff <- height - d_height

  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # qplot(n, div, data = df)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$y <- df$y + height * ((groupidx - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2

  df
}
