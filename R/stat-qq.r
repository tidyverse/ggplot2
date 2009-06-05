StatQq <- proto(Stat, {
  objname <- "qq" 
  desc <- "Calculation for quantile-quantile plot"

  desc_params <- list(
    quantiles = "Quantiles to compute and display",
    dist = "Distribution function to use, if x not specified",
    "..." = "Other arguments passed to distribution function"
  )
  
  default_geom <- function(.) GeomPoint
  default_aes <- function(.) aes(y = ..sample.., x = ..theoretical..)
  required_aes <- c("sample")

  calculate <- function(., data, scales, quantiles = NULL, distribution = qnorm, na.rm = FALSE, ...) {
    
    if (is.null(quantiles)) {
      quantiles <- ppoints(length(data$sample))
    }
    
    theoretical <- safe.call(distribution, list(p = quantiles, ...))
    sample <- quantile(data$sample, probs=quantiles, na.rm=na.rm)
  
    data.frame(sample, theoretical, group = data$group[1])
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

    qplot(sample = y, dist = qt, df = 5)
    qplot(sample = y, quantiles = seq(0,1, length=100))
    
    df <- data.frame(y)
    ggplot(df, aes(sample = y)) + stat_qq()
    ggplot(df, aes(sample = y)) + geom_point(stat = "qq")
    
    # Using to explore the distribution of a variable
    qplot(sample = mpg, data = mtcars)
    qplot(sample = mpg, data = mtcars, colour = factor(cyl))    
  }
  
})
