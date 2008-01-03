StatSummary <- proto(Stat, {
  objname <- "summary" 
  desc <- "Summarise y values at every unique x"
  
  default_geom <- function(.) GeomPointrange
  required_aes <- c("x", "y")
   
  calculate <- function(., data, scales, fun=stat_range, ...) {
    if (is.character(fun)) fun <- match.fun(paste("stat", fun, sep="_"))
    summaryby(data, data$x, fun, ...)
  }
  seealso <- list(
    "geom_errorbar" = "error bars",
    "geom_pointrange" = "range indicated by straight line, with point in the middle",
    "geom_linerange" = "range indicated by straight line",
    "geom_crossbar" = "hollow bar with middle indicated by horizontal line",
    "smean.sdl" = "for description of summary functions provide by Hmisc.  Replace the . with a _ to get the ggplot name",
    "stat_smooth" = "for continuous analog"
  )
  
  examples <- function(.) {
    # Basic operation on a small dataset
    c <- qplot(cyl, mpg, data=mtcars)
    c + stat_summary()
    c + stat_summary(fun="mean_cl_normal", colour="red")
    c + stat_summary(fun="mean_cl_normal", colour="red", geom="crossbar")
    c + stat_summary(fun="mean_cl_normal", colour="red", geom="crossbar", width=0.2)
    c + stat_summary(fun="mean_cl_normal", colour="red", geom="errorbar", width=0.2)
    c + stat_summary(fun="mean_cl_normal", colour="red", geom="linerange")
    c + stat_summary(fun="mean_cl_normal", colour="red", geom="smooth")

    c + stat_summary(fun="mean", colour="red", geom="point")
    c + stat_summary(fun="mean", colour="red", geom="path")
    c + stat_summary(fun="median", colour="red", geom="point")
    
    statsumbar <- function(fun, ...) {
      stat_summary(fun=fun, colour="red", geom="crossbar", width=0.2, ...)
    }
    
    c + statsumbar("mean_cl_boot")
    c + statsumbar("mean_sdl")
    c + statsumbar("mean_sdl", mult=1)
    c + statsumbar("median_hilow")
    
    # Use to augment boxplot
    qplot(color, price, data=diamonds, geom="boxplot") + stat_summary(geom="point",fun="mean")
    qplot(color, price, data=diamonds, geom="boxplot") + stat_summary(aes(group=1), geom="line",fun="mean")
    
    # A bigger dataset, where these summaries are actually useful
    m <- ggplot(movies, aes(x=round(rating), y=votes)) + geom_point()
    
    (m2 <- m + stat_summary(fun="mean_cl_boot", geom="crossbar", colour="red", width=0.3))
    # Notice how the overplotting skews out visual perception of the mean
    # supplementing the raw data with summary statisitcs is _very_ important
  
    # Next, we'll put votes on a log scale.
    # Transforming the scale performs the transforming before the statistic.
    # This means we're calculating the summary on the logged data
    m2 + scale_y_log10()
    # Transforming the coordinate system performs the transforming after the statistic
    # This means we're calculating the summary on the raw data, and stretching
    # the geoms onto the log scale
    m2 + coord_trans(y="log10")
  }
})

summaryby <- function(data, split, summary=yrange, ...) {
  parts <- split(data, factor(split))
  unique <- lapply(parts, function(df) uniquecols(df[setdiff(names(df), c("y"))]))
  
  summary <- lapply(parts, summary, ...)
  
  parts <- mapply(function(x,y) {
    cbind(x, y[rep(1, nrow(x)), ,drop=FALSE])
  }, summary, unique, SIMPLIFY=FALSE)
  do.call("rbind.fill", parts)
}

wrap_hmisc <- function(x, fun, ...) {
  try_require("Hmisc")

  params <- list(...)
  fun.params <- params[intersect(names(formals(fun)), names(params))]
  
  result <- do.call(fun, c(x=list(x), fun.params))
  rename(data.frame(t(result)), c(Median="y", Mean="y", Lower="min", Upper="max"))
}

stat_mean_cl_boot <- function(df, ...) wrap_hmisc(df$y, fun=smean.cl.boot, ...)
stat_mean_cl_normal <- function(df, ...) wrap_hmisc(df$y, fun=smean.cl.normal, ...)
stat_mean_sdl <- function(df, ...) wrap_hmisc(df$y, fun=smean.sdl, ...)
stat_median_hilow <- function(df, ...) wrap_hmisc(df$y, fun=smedian.hilow, ...)

stat_range <- function(data, ...) summaryby(data, data$x, yrange)
stat_mean <- function(data, ...) summaryby(data, data$x, ymean)
stat_median <- function(data, ...) summaryby(data, data$x, ymedian)
stat_max <- function(data, ...) summaryby(data, data$x, ymax)

yrange <- function(df, ...) data.frame(min=min(df$y, na.rm=TRUE), max=max(df$y, na.rm=TRUE))
ymean <- function(df, ...) data.frame(y=mean(df$y, na.rm=TRUE))
ymedian <- function(df, ...) data.frame(y=median(df$y, na.rm=TRUE))
ymax <- function(df, ...) data.frame(y=max(df$y, na.rm=TRUE))
