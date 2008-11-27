StatAbline <- proto(Stat, {
  calculate <- function(., data, scales, intercept = NULL, slope = NULL, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    if (is.null(intercept)) {
      if (is.null(data$intercept)) data$intercept <- 0
    } else {
      data <- data[rep(1, length(intercept)), ]
      data$intercept <- intercept
    }
    if (is.null(slope)) {
      if (is.null(data$slope)) data$slope <- 0
    } else {
      data <- data[rep(1, length(slope)), ]
      data$slope <- slope
    }
    
    unique(data)
  }
  
  objname <- "abline" 
  desc <- "Add a line with slope and intercept"
  icon <- function(.) GeomAbline$icon()
  
  required_aes <- c()
  default_geom <- function(.) GeomAbline
  
  examples <- function(.) {
  }
})

StatVline <- proto(Stat, {
  calculate <- function(., data, scales, intercept = NULL, ...) {
    data <- compute_intercept(data, intercept, "x")
    
    unique(within(data, {
      x    <- intercept
      xend <- intercept
    }))
  }
  
  objname <- "vline" 
  desc <- "Add a vertical line"
  icon <- function(.) GeomVline$icon()
  
  required_aes <- c()
  default_geom <- function(.) GeomVline
  
  examples <- function(.) {
  }
})

StatHline <- proto(Stat, {
  calculate <- function(., data, scales, intercept = NULL, ...) {
    data <- compute_intercept(data, intercept, "y")
    
    unique(within(data, {
      y    <- intercept
      yend <- intercept
    }))
  }
  
  objname <- "hline" 
  desc <- "Add a horizontal line"
  icon <- function(.) GeomHline$icon()
  
  required_aes <- c()
  default_geom <- function(.) GeomHline
  
  examples <- function(.) {
  }
})


# Compute intercept from data
# Compute intercept for vline and hline from data and parameters
# 
# @keywords internal
compute_intercept <- function(data, intercept, var = "x") {
  if (is.null(intercept)) {
    # Intercept comes from data, default to 0 if not set
    if (is.null(data$intercept)) data$intercept <- 0
    
  } else if (is.numeric(intercept)) {
    # Intercept is a numeric vector of positions
    data <- data[rep(1, length(intercept)), ]
    data$intercept <- intercept
    
  } else if (is.character(intercept) || is.function(intercept)) {
    # Intercept is a function
    f <- match.fun(intercept)
    trans <- function(data) transform(data, intercept = f(data[[var]]))
    data <- ddply(data, .(group), trans)
  } else {
    stop("Invalid intercept type: should be a numeric vector, a function", 
         ", or a name of a function", call. = FALSE)
  }
  data
}