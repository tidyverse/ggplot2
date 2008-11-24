# Quick plot.
# Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
# 
# You can use it like you'd use the \code{\link{plot}} function.
# 
# @arguments x values
# @arguments y values
# @arguments z values
# @arguments other arguments passed on to the geom functions
# @arguments data frame to use (optional)
# @arguments faceting formula to use
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
# @arguments the y/x aspect ratio
# @keyword hplot 
# @alias quickplot 
#X # Use data from data.frame
#X qplot(mpg, wt, data=mtcars)
#X qplot(mpg, wt, data=mtcars, colour=cyl)
#X qplot(mpg, wt, data=mtcars, size=cyl)
#X qplot(mpg, wt, data=mtcars, facets=vs ~ am)
#X
#X # Use data from local environment
#X attach(mtcars)
#X qplot(hp, wt)
#X qplot(hp, wt, colour=cyl)
#X qplot(hp, wt, size=cyl)
#X qplot(hp, wt, facets=vs ~ am)
#X
#X qplot(1:10, rnorm(10), colour = runif(10))
#X qplot(1:10, letters[1:10])
#X mod <- lm(mpg ~ wt, data=mtcars)
#X qplot(resid(mod), fitted(mod))
#X qplot(resid(mod), fitted(mod), facets = . ~ vs)
#X
#X f <- function() {
#X    a <- 1:10
#X    b <- a ^ 2
#X    qplot(a, b)
#X } 
#X f()
#X 
#X # Use different geoms
#X qplot(mpg, wt, geom="path")
#X qplot(factor(cyl), wt, geom=c("boxplot", "jitter"))
qplot <- function(x, y = NULL, z=NULL, ..., data, facets = . ~ ., margins=FALSE, geom = "point", stat=list(NULL), position=list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA) {

  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  
  aesthetics <- compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- rename_aes(aesthetics)
  class(aesthetics) <- "uneval"
  
  if (missing(data)) {
    # If data not explicitly specified, will be pulled from workspace
    data <- data.frame()

    # Faceting variables must be in a data frame, so pull those out
    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    facetsdf <- as.data.frame(sapply(facetvars, get))
    if (nrow(facetsdf)) data <- facetsdf
  }

  env <- parent.frame()
  p <- ggplot(data, aesthetics, environment = env)
  
  if (is.formula(facets) && length(facets) == 2) {
    p <- p + facet_wrap(facets)
  } else {
    p <- p + facet_grid(facets = deparse(facets), margins = margins)
  }
  
  if (!is.null(main)) p <- p + opts("title" = main)

  # Add geoms/statistics
  if (is.proto(position)) position <- list(position)
  
  mapply(function(g, s, ps) {
    if(is.character(g)) g <- Geom$find(g)
    if(is.character(s)) s <- Stat$find(s)
    if(is.character(ps)) ps <- Position$find(ps)

    params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    params <- lapply(params, eval, parent.frame(n=1))
    
    p <<- p + layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps)
  }, geom, stat, position)
  
  logv <- function(var) var %in% strsplit(log, "")[[1]]

  if (logv("x")) p <- p + scale_x_log10()
  if (logv("y")) p <- p + scale_y_log10()
  
  if (!is.na(asp)) p <- p + opts(aspect.ratio = asp)
  
  x <- p$scales$get_scales("x")
  y <- p$scales$get_scales("y")
  
  if (!missing(xlab) && !is.null(x)) x$name <- xlab
  if (!missing(ylab) && !is.null(y)) y$name <- ylab
  
  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)
  
  p
}
quickplot <- qplot

# is.constant
# Determine if an expression represents a constant value
# 
# Used by qplot to determine whether a value should be mapped or set
#
# @keywords internal
is.constant <- function(x) {
  sapply(x, function(x) "I" %in% all.names(asOneSidedFormula(x)))
}
