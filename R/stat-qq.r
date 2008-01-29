StatQq <- proto(Stat, {
  objname <- "qq" 
  desc <- "Calculation for quantile-quantile plot"

  desc_params <- list(
    quantiles = "Quantiles to compute and display",
    dist = "Distribution function to use, if x not specified",
    "..." = "Other arguments passed to distribution function"
  )
  
  default_geom <- function(.) GeomPoint
  default_aes <- function(.) aes(y = ..y..)
  required_aes <- c("x", "y")

  calculate <- function(., data, scales, quantiles=ppoints(length(data$x)), distribution=qnorm, na.rm = FALSE, ...) {
    
    if (is.null(data$y)) {
      params <- list(...)
      dist.params <- params[intersect(names(formals(distribution)), names(params))]
      
      qy <- suppressWarnings(do.call(distribution, c(list(quantiles), dist.params)))
    } else {
      qy <- quantile(data$y, probs=quantiles, na.rm=na.rm)
    }
    qx <- quantile(data$x, probs=quantiles, na.rm=na.rm)
  
    data.frame(x=qx, y=qy)
  }
  
  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating))

    m + stat_qq()
    m + stat_qq() + geom_abline()
    m + stat_qq(distribution=qunif, min=1, max=10) + geom_abline()
    m + stat_qq(quantiles = seq(0.01, 0.99, by=0.05), distribution=qunif, min=1, max=10) + geom_abline()
    
    # Let's explore the distribution of ratings
    qplot(movies$rating, geom="histogram", binwidth=0.1)
    
    # Looks pretty normal, so we'll compare to a normal distribution
    # with parameters estimated from the data
    
    distn <- fitdistr(movies$rating, "normal")
    m + stat_qq(distribution=function(x) qnorm(x, distn$estimate[1], sd=distn$estimate[2])) + geom_abline()

    # Not bad, apart from in the tails
    # We'll try something a little longer-tailed
    distt <- suppressWarnings(fitdistr(movies$rating, "t"))
    m + stat_qq(distribution=function(x) qt(x, distt$estimate[3], distt$estimate[1]) * distt$estimate[2]) + geom_abline()
    
    ggplot(movies, aes(x=rating, y=rating * 2)) + stat_qq()
    ggplot(movies, aes(x=rating, y=rating ^ 2)) + stat_qq()
  }
  
})
