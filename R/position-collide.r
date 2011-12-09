# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide <- function(data, width = NULL, name, strategy, check.width = TRUE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data <- within(data, {
        xmin <- x - width / 2
        xmax <- x + width / 2
      })      
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }
    
    # Width determined from data, must be floating point constant 
    widths <- unique(with(data, xmax - xmin))
    widths <- widths[!is.na(widths)]
    if (!zero_range(range(widths))) {
      stop(name, " requires constant width", call. = FALSE)
    }
    width <- widths[1]
  }

  # Reorder by x position, preserving order of group
  data <- data[order(data$xmin, data$group), ]

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]
  
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots. 
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$ymax)) {
    ddply(data, .(xmin), strategy, width = width)
  } else if (!is.null(data$y)) {
    message("ymax not defined: adjusting position using y instead")
    transform(
      ddply(transform(data, ymax = y), .(xmin), strategy, width = width),
      y = ymax
    )
  } else {
    stop("Neither y nor ymax defined")
  }
}

# Stack overlapping intervals.
# Assumes that each set has the same horizontal position
pos_stack <- function(df, width) {
  if (nrow(df) == 1) return(df)
  
  n <- nrow(df) + 1
  y <- with(df, ifelse(is.na(y), 0, y))
  if (all(is.na(df$x))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(y))
  }

  within(df, {
    ymin <- heights[-n]
    ymax <- heights[-1]
    y <- ymax
  })
}

# Stack overlapping intervals and set height to 1.
# Assumes that each set has the same horizontal position.
pos_fill <- function(df, width) {
  within(pos_stack(df, width), {
    ymin <- ymin / max(ymax)
    ymax <- ymax / max(ymax)
    y <- ymax
  })
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

  d_width <- with(df, max(xmax - xmin))    
  diff <- width - d_width
  
  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # qplot(n, div, data = df)
  
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- rep(0, max(df$group))
  groupidx[unique(df$group)] <- 1:n

  df <- within(df, {
    xmin <- xmin + width / n * (groupidx[group] - 1) - diff * (n - 1) / (2 * n)
    xmax <- xmin + d_width / n
    x <-  (xmin + xmax) / 2
  })
}
