bin <- function(x, weights=NULL, binwidth=NULL, breaks=NULL, range=NULL, width=0.9) {
  if (is.null(weights))  weights <- rep(1, length(x))
  weights[is.na(weights)] <- 0

  if (is.null(range))    range <- range(x, na.rm = TRUE, finite=TRUE)
  if (is.null(binwidth)) binwidth <- diff(range) / 30

  if (diff(range) == 0) {
    width <- width
    bins <- x
  } else if (is.numeric(x)) {
    if (is.null(breaks)) breaks <- fullseq(range, binwidth)
    bins <- cut(x, sort(breaks), include.lowest=TRUE)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  } else {
    bins <- factor(x)
    x <- factor(unique(x))
    width <- width
  }

  # if (binwidth < resolution(x)) warning("Binwidth is smaller than the resolution of the data")

  results <- data.frame(
    count = as.numeric(tapply(weights, bins, sum, na.rm=TRUE)),
    x = x,
    width = width
  )
  results <- transform(results,
    density = count / width / sum(count, na.rm=TRUE)
  )
  results <- subset(results, count > 0)
  
  transform(results,
    ncount = count / max(count, na.rm=TRUE),
    ndensity = density / max(density, na.rm=TRUE),
    group = 1
  )
  
}

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
  
  calculate <- function(., data, scales, binwidth=NULL, breaks=NULL, width=0.9, ...) {
    range <- scales$get_scales("x")$frange()

    if (is.null(binwidth) && is.numeric(data$x) && !.$informed) {
      message("stat_bin: bin width unspecified, using 30 bins as default.")
      .$informed <- TRUE
    }
    
    bin(data$x, data$weight, binwidth=binwidth, breaks=breaks, range=range, width=width)
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
  
  default_aes <- function(.) aes(y = ..count..)
  default_geom <- function(.) GeomBar
  
  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating))
    m + stat_bin()
    m + stat_bin(binwidth=0.1)
    m + stat_bin(breaks=seq(4,6, by=0.1))
    # See geom_histogram for more histogram examples
    
    # To create a unit area histogram, use aes(y = ..density..)
    (linehist <- m + stat_bin(aes(y = ..density..), geom="line"))
    linehist + stat_density(colour="blue", fill=NA)
    
    # Also works with categorical variables
    ggplot(movies, aes(x=mpaa)) + stat_bin()
    qplot(mpaa, data=movies, stat="bin")
    
  }
  
})
