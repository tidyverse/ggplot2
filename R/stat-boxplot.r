StatBoxplot <- proto(Stat, {
  objname <- "boxplot" 
  desc <- "Calculate components of box and whisker plot"
  desc_outputs <- list(
    "width" = "width of boxplot",
    "min" = "lower whisker, lower hinge - 1.5 * IQR",
    "lower" = "lower hinge, 25% quantile", 
    "middle" = "median, 50% quantile",
    "upper" = "upper hinge, 75% quantile",
    "max" = "upper whisker, upper hinge + 1.5 * IQR"
  )
  required_aes <- c("x", "y")
  
  icon <- function(.) GeomBoxplot$icon()
  default_geom <- function(.) GeomBoxplot
  
  calculate <- function(., data, scales, width=0.75, ...) {
    x <- data$y
    weights <- if(is.null(data$weights)) 1 else data$weights
    coef <- 1.5
    
    if (length(unique(weights)) != 1) {
      try_require("quantreg")
      stats <- as.numeric(coef(rq(x ~ 1, weight = weights, tau=c(0, 0.25, 0.5, 0.75, 1))))
    } else {
      stats <- as.numeric(quantile(x, c(0, 0.25, 0.5, 0.75, 1)))
    }
    names(stats) <- c("min", "lower", "middle", "upper", "max")
    
    iqr <- diff(stats[c(2, 4)])
    outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
    if (any(outliers)) stats[c(1, 5)] <- range(x[!outliers], na.rm = TRUE)
    
    df <- as.data.frame(as.list(stats))
    df$outliers <- I(list(x[outliers]))
    
    if (is.numeric(data$x)) {
      df$x <- mean(range(data$x))
      df$width <- diff(range(data$x))
    } else {
      df$x <- data$x[1]
      df$width <- width
    }
    df
  }
  
  examples <- function(.) {
    # See geom_boxplot for examples
  }
  
})
