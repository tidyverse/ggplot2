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
  
  calculate_groups <- function(., data, na.rm = FALSE, width = NULL, ...) {
    data <- remove_missing(data, na.rm, c("y", "weight"), name="stat_boxplot")
    data$weight <- nulldefault(data$weight, 1)
    width <- nulldefault(width, resolution(data$x) * 0.75)
        
    .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)
  }
  
  calculate <- function(., data, scales, width=NULL, na.rm = FALSE, coef = 1.5, ...) {
    with(data, {    
      qs <- c(0, 0.25, 0.5, 0.75, 1)
      if (length(unique(weight)) != 1) {
        try_require("quantreg")
        stats <- as.numeric(coef(rq(y ~ 1, weights = weight, tau=qs)))
      } else {
        stats <- as.numeric(quantile(y, qs))
      }
      names(stats) <- c("min", "lower", "middle", "upper", "max")
    
      iqr <- diff(stats[c(2, 4)])
      
      outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)
      if (any(outliers)) stats[c(1, 5)] <- range(y[!outliers], na.rm=TRUE)
    
      if (length(unique(x)) > 1) width <- diff(range(x)) * 0.9
    
      df <- as.data.frame(as.list(stats))
      df$outliers <- I(list(y[outliers]))

      transform(df,
        x = if (is.factor(x)) x[1] else mean(range(x)),
        width = width
      )
    })
  }
  
  examples <- function(.) {
    # See geom_boxplot for examples
  }
  
})
