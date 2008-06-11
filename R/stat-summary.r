StatSummary <- proto(Stat, {
  objname <- "summary" 
  desc <- "Summarise y values at every unique x"
  
  details <- "<p>stat_summary allows for tremendous flexibilty in the specification of summary functions.  The summary function can either operate on a data frame (with argument name data) or on a vector.  A simple vector function is easiest to work with as you can return a single number, but is somewhat less flexible.  If your summary function operates on a data.frame it should return a data frame with variables that the geom can use.</p>"
  
  default_geom <- function(.) GeomPointrange
  required_aes <- c("x", "y")
   
  calculate <- function(., data, scales, fun, ...) {
    
    if (is.character(fun)) {
      sumfun <- paste("sum", fun, sep="_")
      if (exists(sumfun)) fun <- match.fun(sumfun)
    }
    summaryby(data, data$x, auto_wrap(fun), ...)
  }
  seealso <- list(
    "geom_errorbar" = "error bars",
    "geom_pointrange" = "range indicated by straight line, with point in the middle",
    "geom_linerange" = "range indicated by straight line",
    "geom_crossbar" = "hollow bar with middle indicated by horizontal line",
    # "smean.sdl" = "for description of summary functions provide by Hmisc.  Replace the . with a _ to get the ggplot name",
    "stat_smooth" = "for continuous analog"
  )
  
  desc_outputs <- list()
  
  examples <- function(.) {
    # Basic operation on a small dataset
    c <- qplot(cyl, mpg, data=mtcars)
    c + stat_summary()
    
    # The simplest type of summary range takes a vector of x's and returns
    # a single value:
    
    stat_sum_single <- function(fun, geom="point", ...) {
      stat_summary(fun=fun, colour="red", geom=geom, size = 3, ...)      
    }
    
    c + stat_sum_single(mean)
    c + stat_sum_single(mean, geom="line")
    c + stat_sum_single(median)
    c + stat_sum_single(sd)
    
    # More complex summary functions operate on a data.frame, summarising
    # the values of y for a given x (the split into separate data.frames with
    # uniques values of x is performed automatically by stat_summary)
    #
    # A set of useful summary functions is provided from the Hmisc package:
    
    stat_sum_df <- function(fun, geom="crossbar", ...) {
      stat_summary(fun=fun, colour="red", geom=geom, width=0.2, ...)
    }
    
    c + stat_sum_df("mean_cl_boot")
    c + stat_sum_df("mean_sdl")
    c + stat_sum_df("mean_sdl", mult=1)
    c + stat_sum_df("median_hilow")
    c + stat_sum_df("range", geom="linerange")

    # There are lots of different geoms you can use to display the summaries
        
    c + stat_sum_df(fun="mean_cl_normal")
    c + stat_sum_df(fun="mean_cl_normal", geom = "errorbar")
    c + stat_sum_df(fun="mean_cl_normal", geom = "pointrange")
    c + stat_sum_df(fun="mean_cl_normal", geom = "smooth")
        
    # Summaries are much more useful with a bigger data set:
    m <- ggplot(movies, aes(x=round(rating), y=votes)) + geom_point()
    (m2 <- m + stat_summary(fun="mean_cl_boot", geom="crossbar", colour="red", width=0.3))
    # Notice how the overplotting skews out visual perception of the mean
    # supplementing the raw data with summary statisitcs is _very_ important
  
    # Next, we'll look at votes on a log scale.

    # Transforming the scale performs the transforming before the statistic.
    # This means we're calculating the summary on the logged data
    m2 + scale_y_log10()
    # Transforming the coordinate system performs the transforming after the
    # statistic. This means we're calculating the summary on the raw data, 
    # and stretching the geoms onto the log scale.  Compare the widths of the
    # standard errors.
    m2 + coord_trans(y="log10")
  }
})

# Summarise a data.frame by parts
# Summarise a data frame by unique value of x
# 
# This function is used by \code{\link{stat_summary}} to break a 
# data.frame into pieces, summarise each piece, and join the pieces
# back together, retaining original columns unaffected by the summary.
# 
# @argument \code{\link{data.frame}} to summarise
# @argument vector to summarise by
# @argument summary function (must take and return a data.frame)
# @argument other arguments passed on to summary function
# @keyword internal
summaryby <- function(data, split, summary, ...) {
  parts <- split(data, factor(split))
  unique <- lapply(parts, function(df) uniquecols(df[setdiff(names(df), c("y"))]))
  
  summary <- lapply(parts, summary, ...)
  
  parts <- mapply(function(x,y) {
    cbind(x, y[rep(1, nrow(x)), ,drop=FALSE])
  }, summary, unique, SIMPLIFY=FALSE)
  do.call("rbind.fill", parts)
}

# Wrap summary function
# Creates a new function which will operate correctly with \code{\link{stat_summary}}.
# 
# \code{\link{stat_summary}} assumes that summary functions take data.frames
# as input, and return data frames as output.  This function will convert
# a function that takes a vector of values to the correct format.
#
# @arguments function to wrap
# @keyword internal
#X sum_mean <- auto_wrap(mean)
#X sum_mean(data.frame(y = 1:10))
auto_wrap <- function(f) {
  if (is.character(f)) f <- match.fun(f)
  args <- names(formals(f))
  if ("data" %in% args) {
    function(data, ...) f(data, ...)
  } else if ("x" %in% args ) {
    function(df, ...) data.frame(y = safe.call(f, list(x = df$y, ...)))
  } else {
    stop("Functions provided to stat_summary require a parameter named data or x")
  }
}

# Wrap Hmisc summary functions 
# Wrap up a selection of Hmisc to make it easy to use them with \code{\link{stat_summary}}
# 
# @alias sum_mean_cl_boot
# @alias sum_mean_cl_normal
# @alias sum_mean_sdl
# @alias sum_median_hilow
# @alias sum_range
# @alias mean_cl_boot
# @alias mean_cl_normal
# @alias mean_sdl
# @alias median_hilow
# @keyword internal
wrap_hmisc <- function(x, fun, ...) {
  try_require("Hmisc")

  result <- safe.call(fun, list(x=x, ...))
  rename(data.frame(t(result)), c(Median="y", Mean="y", Lower="min", Upper="max"))
}

sum_mean_cl_boot <- function(data, ...) wrap_hmisc(data$y, fun=smean.cl.boot, ...)
sum_mean_cl_normal <- function(data, ...) wrap_hmisc(data$y, fun=smean.cl.normal, ...)
sum_mean_sdl <- function(data, ...) wrap_hmisc(data$y, fun=smean.sdl, ...)
sum_median_hilow <- function(data, ...) wrap_hmisc(data$y, fun=smedian.hilow, ...)
sum_range <- function(data, ...) data.frame(min=min(data$y, na.rm=TRUE), max=max(data$y, na.rm=TRUE))
