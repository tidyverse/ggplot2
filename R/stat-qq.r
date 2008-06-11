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

  calculate <- function(., data, scales, quantiles=ppoints(length(data$sample)), distribution=qnorm, na.rm = FALSE, ...) {
    
    theoretical <- safe.call(distribution, list(p = quantiles, ...))
    sample <- quantile(data$sample, probs=quantiles, na.rm=na.rm)
  
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
    qplot(sample = y, stat="qq", dist=qt, df=5)
    qplot(sample = y, stat="qq", quantiles=seq(0,1, length=100))
    
    df <- data.frame(y)
    ggplot(y, aes(sample = y)) + stat_qq()
    ggplot(y, aes(sample = y)) + geom_point(stat = "qq")
    
    qplot(sample = precip, stat="qq")
  }
  
})
