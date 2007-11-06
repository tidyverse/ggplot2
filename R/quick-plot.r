# Quick plot.
# Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
# You can use it like you'd use the \code{\link{plot}} function.
# 
# FIXME: describe how to get more information
# FIXME: add more examples
# 
# \code{qplot} provides a quick way to create simple plots.
# 
# @arguments x values
# @arguments y values
# @arguments z values
# @arguments other arguments passed on to the geom functions
# @arguments data frame to use (optional)
# @arguments facetting formula to use
# @arguments whether or not margins will be displayed
# @arguments geom to use (can be a vector of multiple names)
# @arguments statistic to use (can be a vector of multiple names)
# @arguments position adjustment to use (can be a vector of multiple names)
# @arguments limits for x axis (aesthetics to range of data)
# @arguments limits for y axis (aesthetics to range of data)
# @arguments which variables to log transform ("x", "y", or "xy")
# @arguments character vector or expression for plot title
# @arguments character vector or expression for x axis label
# @arguments character vector or expression for y axis label
# @keyword hplot 
#X # Use data from data.frame
#X qplot(mpg, wt, data=mtcars)
#X qplot(mpg, wt, data=mtcars, colour=cyl)
#X qplot(mpg, wt, data=mtcars, size=cyl)
#X qplot(mpg, wt, data=mtcars, facets=vs ~ am)
#X
#X # Use data from workspace environment
#X attach(mtcars)
#X qplot(mpg, wt)
#X qplot(mpg, wt, colour=cyl)
#X qplot(mpg, wt, size=cyl)
#X qplot(mpg, wt, facets=vs ~ am)
#X
#X # Use different geoms
#X qplot(mpg, wt, geom="path")
#X qplot(factor(cyl), wt, geom=c("boxplot", "jitter"))
quickplot <- qplot <- function(x, y = NULL, z=NULL, ..., data, facets = . ~ ., margins=FALSE, geom = "point", stat=list(NULL), position=list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y))) {

  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  aesthetics <- compact(arguments[.all_aesthetics])
  class(aesthetics) <- "uneval"
  
  # Create data if not explicitly specified
  if (missing(data)) {
    data <- as.data.frame(lapply(aesthetics, eval, parent.frame(n=2)))

    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    facetsdf <- as.data.frame(sapply(facetvars, get))
    if (nrow(facetsdf)) data <- cbind(data, facetsdf)
  } else {
    if (!is.data.frame(data)) stop("data is not a data.frame")
    if (ncol(data) == 0) stop("data has no columns")
    if (nrow(data) == 0) stop("data has no rows")
  }

  p <- ggplot(data, aesthetics) + facet_grid(facets=deparse(facets), margins=margins)
  
  if (!is.null(main)) p$title <- main
  if (!is.null(xlab)) p$xlabel <- xlab
  if (!is.null(ylab)) p$ylabel <- ylab

  # Add geoms/statistics
  if (is.proto(position)) position <- list(position)
  
  mapply(function(g, s, ps) {
    if(is.character(g)) g <- Geom$find(g)
    if(is.character(s)) s <- Stat$find(s)
    if(is.character(ps)) ps <- Position$find(ps)

    params <- arguments[setdiff(names(arguments), c(.all_aesthetics, argnames))]
    
    p <<- p + layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps)
  }, geom, stat, position)
  
  logv <- function(var) var %in% strsplit(log, "")[[1]]

  if (logv("x")) p <- p + scale_x_log10()
  if (logv("y")) p <- p + scale_y_log10()
  
  if (!missing(xlab)) assign("name", xlab, envir=p$scales$get_scales("x"))
  if (!missing(ylab)) assign("name", ylab, envir=p$scales$get_scales("y"))
  
  if (!missing(xlim)) assign("limits", xlim, envir=p$scales$get_scales("x"))
  if (!missing(ylim)) assign("limits", ylim, envir=p$scales$get_scales("y"))
  
  p
}