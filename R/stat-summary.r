StatSummary <- proto(Stat, {
  objname <- "summary" 
  desc <- "Summarise y values at every unique x"
  
  details <- "<p>stat_summary allows for tremendous flexibilty in the specification of summary functions.  The summary function can either operate on a data frame (with argument name data) or on a vector.  A simple vector function is easiest to work with as you can return a single number, but is somewhat less flexible.  If your summary function operates on a data.frame it should return a data frame with variables that the geom can use.</p>"
  
  default_geom <- function(.) GeomPointrange
  required_aes <- c("x", "y")
   
  calculate_groups <- function(., data, scales, fun.data = NULL, fun.y = NULL, fun.ymax = NULL, fun.ymin = NULL, ...) {
    
    if (!missing(fun.data)) {
      # User supplied function that takes complete data frame as input
      fun.data <- match.fun(fun.data)
      fun <- function(df, ...) {
        fun.data(df$y, ...)
      }
    } else {
      # User supplied individual vector functions
      fs <- compact(list(ymin = fun.ymin, y = fun.y, ymax = fun.ymax))
      
      fun <- function(df, ...) {
        res <- llply(fs, function(f) do.call(f, list(df$y, ...)))
        names(res) <- names(fs)
        as.data.frame(res)
      }
    }
    summarise_by_x(data, fun, ...)
  }
  seealso <- list(
    "geom_errorbar" = "error bars",
    "geom_pointrange" = "range indicated by straight line, with point in the middle",
    "geom_linerange" = "range indicated by straight line",
    "geom_crossbar" = "hollow bar with middle indicated by horizontal line",
    # "smean.sdl" = "for description of summary functions provide by Hmisc.  Replace the . with a _ to get the ggplot name",
    "stat_smooth" = "for continuous analog"
  )
  
  desc_params <- list(
    fun.data = "Complete summary function.  Should take data frame as input and return data frame as output.",
    fun.ymin = "ymin summary function (should take numeric vector and return single number)",
    fun.y = "ym summary function (should take numeric vector and return single number)",
    fun.ymax = "ymax summary function (should take numeric vector and return single number)"
  )
  
  
  desc_outputs <- list()
  
  examples <- function(.) {
    # Basic operation on a small dataset
    c <- qplot(cyl, mpg, data=mtcars)
    c + stat_summary()

    # You can supply individual functions to summarise the value at 
    # each x:
    
    stat_sum_single <- function(fun, geom="point", ...) {
      stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)      
    }
    
    c + stat_sum_single(mean)
    c + stat_sum_single(mean, geom="line")
    c + stat_sum_single(median)
    c + stat_sum_single(sd)
    
    c + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, 
      colour = "red")
    
    # Alternatively, you can supply a function that operates on a data.frame.
    # A set of useful summary functions is provided from the Hmisc package:
    
    stat_sum_df <- function(fun, geom="crossbar", ...) {
      stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)
    }
    
    c + stat_sum_df("mean_cl_boot")
    c + stat_sum_df("mean_sdl")
    c + stat_sum_df("mean_sdl", mult=1)
    c + stat_sum_df("median_hilow")

    # There are lots of different geoms you can use to display the summaries
        
    c + stat_sum_df(fun.data="mean_cl_normal")
    c + stat_sum_df(fun.data="mean_cl_normal", geom = "errorbar")
    c + stat_sum_df(fun.data="mean_cl_normal", geom = "pointrange")
    c + stat_sum_df(fun.data="mean_cl_normal", geom = "smooth")
        
    # Summaries are much more useful with a bigger data set:
    m <- ggplot(movies, aes(x=round(rating), y=votes)) + geom_point()
    m2 <- m + 
       stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", 
         colour = "red", width = 0.3)
    m2
    # Notice how the overplotting skews off visual perception of the mean
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
summarise_by_x <- function(data, summary, ...) {
  summary <- ddply(data, .(group, x), summary)
  unique <- ddply(data, .(group, x), uniquecols)
  unique$y <- NULL
  
  merge(summary, unique, by = c("x", "group"))
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

  result <- safe.call(fun, list(x = x, ...))
  rename(
    data.frame(t(result)), 
    c(Median = "y", Mean = "y", Lower = "ymin", Upper = "ymax")
  )
}

mean_cl_boot <- function(x, ...) 
  wrap_hmisc(x, fun = smean.cl.boot, ...)
mean_cl_normal <- function(x, ...) 
  wrap_hmisc(x, fun = smean.cl.normal, ...)
mean_sdl <- function(x, ...) 
  wrap_hmisc(x, fun = smean.sdl, ...)
median_hilow <- function(x, ...) 
  wrap_hmisc(x, fun = smedian.hilow, ...)