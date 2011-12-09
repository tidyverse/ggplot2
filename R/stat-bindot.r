#' Bin data.
#' 
#' Missing values are currently silently dropped.
#'
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#'   data
#' @param breaks Actual breaks to use.  Overrides bin width and origin 
#' @param origin Origin of first bin 
#' @param width Width of bars when used with categorical data 
#' @param right Should intervals be closed on the right (a, b], or not [a, b) 
#' @param drop If TRUE, remove all bins with zero counts
#' @return New data frame with additional columns:
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
#' @export
#' @examples
#' simple <- data.frame(x = rep(1:10, each = 2))
#' base <- ggplot(simple, aes(x))
#' # By default, right = TRUE, and intervals are of the form (a, b]
#' base + stat_bin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
#' # If right = FALSE intervals are of the form [a, b)
#' base + stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")
#' 
#' m <- ggplot(movies, aes(x=rating))
#' m + stat_bin()
#' m + stat_bin(binwidth=0.1)
#' m + stat_bin(breaks=seq(4,6, by=0.1))
#' # See geom_histogram for more histogram examples
#' 
#' # To create a unit area histogram, use aes(y = ..density..)
#' (linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
#'   geom="line", position="identity"))
#' linehist + stat_density(colour="blue", fill=NA)
#' 
#' # Also works with categorical variables
#' ggplot(movies, aes(x=mpaa)) + stat_bin()
#' qplot(mpaa, data=movies, stat="bin")
stat_bindot <- function (mapping = NULL, data = NULL, geom = "dotplot", position = "stack",
width = 0.9, binstataxis = "x", binmethod = "dotdensity", drop = FALSE, right = TRUE, ...) {
  StatBindot$new(mapping = mapping, data = data, geom = geom, position = position, 
  width = width, binstataxis = binstataxis, binmethod = binmethod,
  drop = drop, right = right, ...)
}

#TODO: test weighted binning for both methods, and error on non-integer weights
#TODO: check that binmethod is valid
#TODO: add smoothing from Wilkinson paper?

StatBindot <- proto(Stat, {
  objname <- "bindot"
  informed <- FALSE
  
  calculate_groups <- function(., data, ...) {
    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }
  
  calculate <- function(., data, scales, binwidth=NULL, binstataxis="x", binmethod = "dotdensity",
                        origin=NULL, breaks=NULL, width=0.9, drop = FALSE, right = TRUE, ...) {

    # This function taken from integer help page
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

    # Check that weights are whole numbers (for dots, weights must be whole)
    if (!is.null(data$weight) && !all(is.wholenumber(data$weight)))
        stop("Weights for stat_bindot must be whole numbers.")

    if (binstataxis=="x") {
      range  <- scale_dimension(scales$x)
      values <- data$x
    } else if (binstataxis=="y") {
      range  <- scale_dimension(scales$y)
      values <- data$y
    }

    if (is.null(breaks) && is.null(binwidth) && !is.integer(values) && !.$informed) {
      message("stat_bindot: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }
    
    data <- bindot(values, data$weight, binwidth=binwidth,
                binmethod=binmethod, origin=origin, breaks=breaks, range=range,
                width=width, drop = drop, right = right)

    if (binstataxis=="x") {
      # For x binning, the width of the geoms is same as the width of the bin
      data$width <- data$binwidth
    } else if (binstataxis=="y") {
      # bindot returns the data values in a column named x; change the name to y
      names(data)[names(data)=="x"] <- "y"
    }

    return(data)
  }

  icon <- function(.) GeomHistogram$icon()
#TODO: Are the aes things necessary?
  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomDotplot
  
})

# TODO: make use of some of these parameters in dotdensity??
# Bin the values.
# histodot just uses stat_bin
# dotdensity is from Wilkinson (1999)
bindot <- function(x, weight=NULL, binwidth=NULL, binmethod=binmethod, origin=NULL, breaks=NULL, range=NULL, width=0.9, drop = FALSE, right = TRUE) {
  
  if(binmethod == "histodot") {
    # Use the function from stat_bin
    results <- bin(x=x, weight=weight, binwidth=binwidth, origin=origin, breaks=breaks,
                   range=range, width=width, drop=drop, right=right)

    # Change "width" column to "binwidth" for consistency
    names(results)[names(results)=="width"] <- "binwidth"
    return(results)

  } else if (binmethod == "dotdensity") {

    if (length(na.omit(x)) == 0) return(data.frame())
    if (is.null(weight))  weight <- rep(1, length(x))
    weight[is.na(weight)] <- 0

    if (is.null(range))    range <- range(x, na.rm = TRUE, finite=TRUE)
    if (is.null(binwidth)) binwidth <- diff(range) / 30

    # TODO: deal with fuzziness
    # Sort weight and x, by x
    weight <- weight[order(x)]
    x      <- x[order(x)]

    cbin <- 0                            # Current bin ID
    bins    <- integer(length=length(x)) # The bin ID for each observation
    binend  <- -Inf                      # End position of current bin (scan left to right)

    # Scan list and put dots in bins
    for (i in 1:length(x)) {
        # If past end of bin, start a new bin at this point
        if (x[i] >= binend) {
            binend <- x[i] + binwidth
            cbin <- cbin + 1
        }

        bins[i] <- cbin
    }

    results <- data.frame(
      count = as.numeric(tapply(weight, bins, sum, na.rm=TRUE)),
      # Center of each bin - centering is not weighted
      x = as.numeric(tapply(x, bins, FUN = function(a) { (min(a) + max(a)) / 2 })),
      binwidth = binwidth
    )

    if (sum(results$count, na.rm = TRUE) == 0) {
      return(results)
    }

# TODO: check that density value is correct. (should it sum to 1? Because it doesn't always)
    res <- within(results, {
      count[is.na(count)] <- 0
      density <- count / width / sum(abs(count), na.rm=TRUE)
      ncount <- count / max(abs(count), na.rm=TRUE)
      ndensity <- density / max(abs(density), na.rm=TRUE)
    })
    if (drop) res <- subset(res, count > 0)

    return(res)
  }
}

# Generate sequence of fixed size intervals covering range
# All locations are multiples of size
# 
# @param range
# @param interval size
# @keyword internal
# @seealso \code{\link{reshape}{round_any}}
fullseq <- function(range, size, pad = FALSE) {
  if (diff(range) < 1e-6) return(c(range[1] - size / 2, range[1] + size / 2))
  
  x <- seq(
    round_any(range[1], size, floor), 
    round_any(range[2], size, ceiling), 
    by=size
  )
  
  if (pad) {
    # Add extra bin on bottom and on top, to guarantee that we cover complete
    # range of data, whether right = T or F
    c(min(x) - size, x, max(x) + size)
  } else {
    x
  }
  
}

