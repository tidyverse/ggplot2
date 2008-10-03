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
      return(ddply(data, .(x), function(df) strategy(df, width = 0)))
    }
    
    # Width determined from data, must be floating point constant 
    widths <- unique(with(data, xmax - xmin))
    if (check.width && length(widths) > 1 && sd(widths) > 1e-6) {
      stop(name, " requires constant width", call. = FALSE)
    }
    width <- widths[1]
  }
  
  # Check for overlap
  data <- data[order(data$xmin), ]
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  if (any(diff(intervals) < -1e-6)) {
    stop(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots. 
    # The American Statistician, 1999.] should be used
  }

  ddply(data, .(xmin), function(df) strategy(df, width = width))
}

# Stack overlapping intervals
# Assumes that each set has the same horizontal position
# 
# @keywords internal
pos_stack <- function(df, width) {
  n <- nrow(df) + 1
  y <- with(df, ifelse(is.na(y), 0, y))
  heights <- c(0, cumsum(y))

  within(df, {
    ymin <- heights[-n]
    ymax <- heights[-1]
  })
}

# Stack overlapping intervals and set height to 1
# Assumes that each set has the same horizontal position
# 
# @keywords internal
pos_fill <- function(df, width) {
  within(pos_stack(df, width), {
    ymin <- ymin / max(ymax)
    ymax <- ymax / max(ymax)
  })
}

# Dodge overlapping interval
# Assumes that each set has the same horizontal position
# 
# @keywords internal
pos_dodge <- function(df, width) {
  n <- nrow(df)
  if (n == 1) return(df)
  
  d_width <- with(df, max(xmax - xmin))
  diff <- width - d_width

  within(df, {
    xmin <- xmin + width / n * (seq_len(n) - 1) - diff / 4
    xmax <- xmin + d_width / n
    x <- (xmin + xmax) / 2
  })
}