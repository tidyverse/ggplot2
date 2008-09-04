collide <- function(data, width = NULL, name, strategy) {
  
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    data <- within(data, {
      xmin <- x - width / 2
      xmax <- x + width / 2
    })
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      return(ddply(data, .(x), function(df) strategy(df, width = 0)))
    }
    
    # Width determined from data, must be floating point constant 
    widths <- unique(with(data, xmax - xmin))
    if (length(widths) > 1 && sd(widths) > 1e-6) {
      stop(name, " requires constant width", call. = FALSE)
    }
    width <- widths[1]
  }
  
  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  if (any(diff(intervals) < -1e-6)) {
    stop(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots. 
    # The American Statistician, 1999.] should be used
  }

  ddply(data, .(xmin), function(df) strategy(df, width = width))
}

# Assumes that each set has the same horizontal position
pos_stack <- function(df, width) {
  n <- nrow(df) + 1
  y <- with(df, ifelse(is.na(y), 0, y))
  heights <- c(0, cumsum(y))

  within(df, {
    ymin <- heights[-n]
    ymax <- heights[-1]
  })
}

pos_fill <- function(df, width) {
  within(stack(df, width), {
    ymin <- ymin / max(ymax)
    ymax <- ymax / max(ymax)
  })
}

# Assumes that each set has the same horizontal position
pos_dodge <- function(df, width) {
  n <- nrow(df)

  within(df, {
    xmin <- xmin + width / n * (seq_len(n) - 1)
    xmax <- xmin + width / n
    x <- (xmin + xmax) / 2
  })
}