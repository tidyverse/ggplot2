StatQq <- proto(Stat, {
  objname <- "qq" 
  desc <- "Calculation for quantile-quantile plot"

  desc_params <- list(
    quantiles = "Quantiles to compute and display",
    dist = "Distribution function to use, if x not specified",
    dparams = "Parameters for distribution function", 
    "..." = "Other arguments passed to distribution function"
  )
  
  default_geom <- function(.) GeomPoint
  default_aes <- function(.) aes(y = ..sample.., x = ..theoretical..)
  required_aes <- c("sample")

  calculate <- function(., data, scales, quantiles = NULL, distribution = qnorm, dparams = list(), na.rm = FALSE) {
    data <- remove_missing(data, na.rm, "sample", name = "stat_qq")    

    sample <- sort(data$sample)
    n <- length(sample)
    
    # Compute theoretical quantiles
    if (is.null(quantiles)) {
      quantiles <- ppoints(n)
    } else {
      stopifnot(length(quantiles) == n)
    }

    theoretical <- safe.call(distribution, c(list(p = quantiles), dparams))
  
    data.frame(sample, theoretical)
  }
  
  desc_outputs <- list(
    sample = "sample quantiles", 
    theoretical = "theoretical quantiles"
  )
  
  examples <- function(.) {
    # From ?qqplot
    y <- rt(200, df = 5)
    qplot(sample = y, stat="qq")

    # qplot is smart enough to use stat_qq if you use sample
    qplot(sample = y)
    qplot(sample = precip)

    qplot(sample = y, dist = qt, dparams = list(df = 5))
    
    df <- data.frame(y)
    ggplot(df, aes(sample = y)) + stat_qq()
    ggplot(df, aes(sample = y)) + geom_point(stat = "qq")
    
    # Use fitdistr from MASS to estimate distribution params
    params <- as.list(MASS::fitdistr(y, "t")$estimate)
    ggplot(df, aes(sample = y)) + stat_qq(dist = qt, dparam = params)
    
    # Using to explore the distribution of a variable
    qplot(sample = mpg, data = mtcars)
    qplot(sample = mpg, data = mtcars, colour = factor(cyl))    
  }
  
})
