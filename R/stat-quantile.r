StatQuantile <- proto(Stat, {
  objname <- "quantile" 
  desc <- "Continuous quantiles"
  icon <- function(.) GeomQuantile$icon()

  desc_params <- list(
    quantiles = "conditional quantiles of y to calculate and display",
    formula = "formula relating y variables to x variables",
    xseq = "exact points to evaluate smooth at, overrides n"
  )
  desc_outputs <- list(
    quantile = "quantile of distribution"
  )
  
  default_geom <- function(.) GeomQuantile
  default_aes <- function(.) aes()
  required_aes <- c("x", "y")

  calculate <- function(., data, scales, quantiles=c(0.25, 0.5, 0.75), formula=y ~ x, xseq = NULL, method="rq", na.rm = FALSE, ...) {
    try_require("quantreg")
    if (is.null(data$weight)) data$weight <- 1 

    if (is.null(xseq)) xseq <- seq(min(data$x, na.rm=TRUE), max(data$x, na.rm=TRUE), length=100)

    data <- as.data.frame(data)
    data <- remove_missing(data, na.rm, c("x", "y"), name = "stat_quantile")
    
    method <- match.fun(method)
    model <- method(formula, data=data, tau=quantiles, weight=weight, ...)

    yhats <- stats::predict(model, data.frame(x=xseq), type="matrix")
    
    quantile <- rep(quantiles, each=length(xseq))
    data.frame(
      y = as.vector(yhats), 
      x = xseq, 
      quantile = quantile,
      group = paste(data$group[1], quantile, sep = "-")
    )
  }
  
  examples <- function(.) {
    msamp <- movies[sample(nrow(movies), 1000), ]
    m <- ggplot(msamp, aes(y=rating, x=year)) + geom_point() 
    m + stat_quantile()
    m + stat_quantile(quantiles = 0.5)
    m + stat_quantile(quantiles = seq(0.1, 0.9, by=0.1))

    # Doesn't work.  Not sure why.
    # m + stat_quantile(method = rqss, formula = y ~ qss(x), quantiles = 0.5)

    # Add aesthetic mappings
    m + stat_quantile(aes(weight=votes))

    # Change scale
    m + stat_quantile(aes(colour = ..quantile..), quantiles = seq(0.05, 0.95, by=0.05))
    m + stat_quantile(aes(colour = ..quantile..), quantiles = seq(0.05, 0.95, by=0.05)) +
      scale_colour_gradient2(midpoint=0.5, low="green", mid="yellow", high="green")

    # Set aesthetics to fixed value
    m + stat_quantile(colour="red", size=2, linetype=2)
    
    # Use qplot instead
    qplot(year, rating, data=movies, geom="quantile")

  }
  
})
