# Bin data
# This function powers \code{\link{stat_bin}}R
#
# @keyword internal
bin <- function(x, weight=NULL, binwidth=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9, drop = FALSE) {
  
  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0

  if (is.null(range))    range <- range(x, na.rm = TRUE, finite=TRUE)
  if (is.null(binwidth)) binwidth <- diff(range) / 30

  if (is.integer(x)) {
    bins <- x
    x <- sort(unique(bins))
    width <- width    
  } else if (diff(range) == 0) {
    width <- width
    bins <- x
  } else { # if (is.numeric(x)) 
    if (is.null(breaks)) {
      if (is.null(origin)) {
        breaks <- fullseq(range, binwidth)        
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }
    bins <- cut(x, sort(breaks), include.lowest=TRUE)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  }

  results <- data.frame(
    count = as.numeric(tapply(weight, bins, sum, na.rm=TRUE)),
    x = x,
    width = width
  )

  res <- within(results, {
    count[is.na(count)] <- 0
    density <- count / width / sum(count, na.rm=TRUE)
    ncount <- count / max(count, na.rm=TRUE)
    ndensity <- density / max(density, na.rm=TRUE)
  })
  if (drop) res <- subset(res, count > 0)
  res
}

# Generate sequence of fixed size intervals covering range
# All locations are multiples of size
# 
# @arguments range
# @arguments interval size
# @keyword internal
# @seealso \code{\link{reshape}{round_any}}
fullseq <- function(range, size) {
  seq(
    round_any(range[1], size, floor), 
    round_any(range[2], size, ceiling), 
    by=size
  )
}

StatBin <- proto(Stat, {
  informed <- FALSE
  
  calculate_groups <- function(., data, ...) {
    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }
  
  calculate <- function(., data, scales, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9, drop = FALSE, ...) {
    range <- scales$get_scales("x")$output_set()

    if (is.null(breaks) && is.null(binwidth) && !is.integer(data$x) && !.$informed) {
      message("stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }
    
    bin(data$x, data$weight, binwidth=binwidth, origin=origin, breaks=breaks, range=range, width=width, drop = FALSE)
  }

  objname <- "bin" 
  desc <- "Bin data"
  icon <- function(.) GeomHistogram$icon()
  desc_params <- list(
    binwidth = "Bin width to use. Defaults to 1/30 of the range of the data.",
    breaks = "Actual breaks to use.  Overrides bin width",
    width = "Width of bars when used on categorical data"
  )
  desc_outputs <- list(
    count = "number of points in bin",
    density = "density of points in bin, scaled to integrate to 1",
    ncount = "count, scaled to maximum of 1",
    ndensity = "density, scaled to maximum of 1"
  )
  details <- "<p>Missing values are currently silently dropped.</p>"
  
  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomBar
  
  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating))
    m + stat_bin()
    m + stat_bin(binwidth=0.1)
    m + stat_bin(breaks=seq(4,6, by=0.1))
    # See geom_histogram for more histogram examples
    
    # To create a unit area histogram, use aes(y = ..density..)
    (linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
      geom="line", position="identity"))
    linehist + stat_density(colour="blue", fill=NA)
    
    # Also works with categorical variables
    ggplot(movies, aes(x=mpaa)) + stat_bin()
    qplot(mpaa, data=movies, stat="bin")
    
  }
  
})
