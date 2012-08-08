#' Count number of observation in rectangular bins.
#' 
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "bin2d")}
#'
#' @inheritParams stat_identity
#' @param bins numeric vector giving number of bins in both vertical and 
#'   horizontal directions. Set to 30 by default.
#' @param drop if \code{TRUE} removes all cells with 0 counts.
#' @seealso \code{\link{stat_binhex}} for hexagonal binning
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, price))
#' d + stat_bin2d()
#' d + geom_bin2d()
#' 
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + stat_bin2d(bins = 10)
#' d + stat_bin2d(bins = 30)
#' 
#' # Or by specifying the width of the bins
#' d + stat_bin2d(binwidth = c(1, 1000))
#' d + stat_bin2d(binwidth = c(.1, 500))
#' 
#' # Or with a list of breaks
#' x <- seq(min(diamonds$carat), max(diamonds$carat), by = 0.1)
#' y <- seq(min(diamonds$price), max(diamonds$price), length = 50)
#' d + stat_bin2d(breaks = list(x = x, y = y))
#' 
#' # With qplot
#' qplot(x, y, data = diamonds, geom="bin2d", 
#'   xlim = c(4, 10), ylim = c(4, 10))
#' qplot(x, y, data = diamonds, geom="bin2d", binwidth = c(0.1, 0.1),
#'   xlim = c(4, 10), ylim = c(4, 10))
#' }
stat_bin2d <- function (mapping = NULL, data = NULL, geom = NULL, position = "identity", 
bins = 30, drop = TRUE, ...) {
  
  StatBin2d$new(mapping = mapping, data = data, geom = geom, position = position, 
  bins = bins, drop = drop, ...)
}

StatBin2d <- gg(proto(Stat, {
  objname <- "bin2d"

  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomRect

  calculate <- function(., data, scales, binwidth = NULL, bins = 30, breaks = NULL, origin = NULL, drop = TRUE, ...) {
    
    range <- list(
      x = scale_dimension(scales$x, c(0, 0)),
      y = scale_dimension(scales$y, c(0, 0))
    )
    
    # Determine origin, if omitted
    if (is.null(origin)) {
      origin <- c(NA, NA)
    } else {
      stopifnot(is.numeric(origin))
      stopifnot(length(origin) == 2)
    }    
    originf <- function(x) if (is.integer(x)) -0.5 else min(x, na.rm = TRUE)
    if (is.na(origin[1])) origin[1] <- originf(data$x)
    if (is.na(origin[2])) origin[2] <- originf(data$y)
    
    # Determine binwidth, if omitted
    if (is.null(binwidth)) {
      binwidth <- c(NA, NA)
      if (is.integer(data$x)) {
        binwidth[1] <- 1
      } else {
        binwidth[1] <- diff(range$x) / bins
      }
      if (is.integer(data$y)) {
        binwidth[2] <- 1
      } else {
        binwidth[2] <- diff(range$y) / bins
      }      
    }
    stopifnot(is.numeric(binwidth))
    stopifnot(length(binwidth) == 2)
    
    # Determine breaks, if omitted
    if (is.null(breaks)) {
      breaks <- list(
        seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
        seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
      )
    } else {
      stopifnot(is.list(breaks))
      stopifnot(length(breaks) == 2)      
      stopifnot(all(sapply(breaks, is.numeric)))
    }
    names(breaks) <- c("x", "y")
    
    xbin <- cut(data$x, sort(breaks$x), include.lowest = TRUE)
    ybin <- cut(data$y, sort(breaks$y), include.lowest = TRUE)
    
    if (is.null(data$weight)) data$weight <- 1
    
    counts <- as.data.frame(
      xtabs(weight ~ xbin + ybin, data), responseName = "count")
    if (drop) counts <- subset(counts, count > 0)
    
    within(counts,{
      xint <- as.numeric(xbin)
      xmin <- breaks$x[xint]
      xmax <- breaks$x[xint + 1]

      yint <- as.numeric(ybin)
      ymin <- breaks$y[yint]
      ymax <- breaks$y[yint + 1]
  
      density <- count / sum(count, na.rm = TRUE)
    })
  }  
}))
