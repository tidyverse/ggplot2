#' Bin data for dot plot.
#' 
#' Missing values are currently silently dropped.
#' If weights are used, they must be integer values.
#'
#' @param binaxis The axis to bin along, "x" (default) or "y"
#' @param binmethod "dotdensity" (default) for dot-density binning, or
#'   "histodot" for fixed bin widths (like stat_bin)
#' @param binwidth When \code{binmethod} is "dotdensity, this specifies maximum bin width.
#'   When \code{binmethod} is "histodot", this specifies bin width.
#'   Defaults to 1/30 of the range of the data
#' @param binpositions When \code{binmethod} is "dotdensity", "bygroup" (default)
#'   determines positions of the bins for each group separately. "all" determines
#'   positions of the bins with all the data taken together; this is used for
#'   aligning dot stacks across multiple groups.
#' @param breaks When \code{binmethod} is "histodot", actual breaks to use.  
#'   Overrides bin width and origin 
#' @param origin When \code{binmethod} is "histodot", origin of first bin 
#' @param width When \code{binmethod} is "histodot", width of bars when used 
#'   with categorical data 
#' @param right When \code{binmethod} is "histodot", should intervals be closed
#'   on the right (a, b], or not [a, b) 
#' @param drop If TRUE, remove all bins with zero counts
#'
#' @return New data frame with additional columns:
#'   \item{x}{center of each bin, if binaxis is "x"}
#'   \item{y}{center of each bin, if binaxis is "x"}
#'   \item{binwidth}{max width of each bin if binmethod is "dotdensity";
#'     width of each bin if binmethod is "histodot"}
#'   \item{count}{number of points in bin}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{density}{density of points in bin, scaled to integrate to 1, 
#'     if binmethod is "histodot"}
#'   \item{ndensity}{density, scaled to maximum of 1, if binmethod is "histodot"}
#' @export
#' @examples
#' # See geom_dotplot for examples
#'
stat_bindot <- function (mapping = NULL, data = NULL, geom = "dotplot", position = "identity",
width = 0.9, binaxis = "x", binmethod = "dotdensity", binpositions = "bygroup",
drop = FALSE, right = TRUE, ...) {
  StatBindot$new(mapping = mapping, data = data, geom = geom, position = position,
  width = width, binaxis = binaxis, binmethod = binmethod, binpositions = binpositions,
  drop = drop, right = right, ...)
}


StatBindot <- proto(Stat, {
  objname <- "bindot"
  informed <- FALSE
  
  calculate_groups <- function(., data, binwidth = NULL, binaxis = "x",
                        binmethod = "dotdensity", binpositions = "bygroup", ...) {
    .$informed <- FALSE

    # If using dotdensity and binning over all, we need to find the bin centers
    # for all data before it's split into groups.
    if (binmethod == "dotdensity" && binpositions == "all") {
      if (binaxis == "x") {
        newdata <- densitybin(x = data$x, weight = data$weight, binwidth = binwidth,
                      binmethod = binmethod)

        data    <- arrange(data, x)
        newdata <- arrange(newdata, x)

      } else if (binaxis == "y") {
        newdata <- densitybin(x = data$y, weight = data$weight, binwidth = binwidth,
                    binmethod = binmethod)

        data    <- arrange(data, y)
        newdata <- arrange(newdata, x)
      }

      data$bin       <- newdata$bin
      data$binwidth  <- newdata$binwidth
      data$weight    <- newdata$weight
      data$bincenter <- newdata$bincenter

    }

    .super$calculate_groups(., data, binwidth = binwidth, binaxis = binaxis,
            binmethod = binmethod, binpositions = binpositions, ...)
  }


  calculate <- function(., data, scales, binwidth = NULL, binaxis = "x",
                        binmethod = "dotdensity", binpositions = "bygroup",
                        origin = NULL, breaks = NULL, width = 0.9, drop = FALSE,
                        right = TRUE, ...) {

    # This function taken from integer help page
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    }

    # Check that weights are whole numbers (for dots, weights must be whole)
    if (!is.null(data$weight) && any(!is.wholenumber(data$weight)) &&
        any(data$weight < 0)) {
      stop("Weights for stat_bindot must be nonnegative integers.")
    }

    if (binaxis == "x") {
      range  <- scale_dimension(scales$x)
      values <- data$x
    } else if (binaxis == "y") {
      range  <- scale_dimension(scales$y)
      values <- data$y
    }

    if (is.null(breaks) && is.null(binwidth) && !is.integer(values) && !.$informed) {
      message("stat_bindot: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }


  if(binmethod == "histodot") {
    # Use the function from stat_bin
    data <- bin(x = values, weight = data$weight, binwidth = binwidth, origin = origin,
                breaks=breaks, range = range, width = width, drop = drop, right = right)

    # Change "width" column to "binwidth" for consistency
    names(data)[names(data) == "width"] <- "binwidth"

  } else if (binmethod == "dotdensity") {

    # If bin centers are found by group instead of by all, find the bin centers
    # (If binpositions=="all", then we'll already have bin centers.)
    if (binpositions == "bygroup")
      data <- densitybin(x = values, weight = data$weight, binwidth = binwidth,
                binmethod = binmethod, range = range)
    
    # Collapse each bin and get a count
    data <- ddply(data, .(bincenter), summarise, binwidth = binwidth[1], count = sum(weight))

    if (sum(data$count, na.rm = TRUE) != 0) {
      data$count[is.na(data$count)] <- 0
      data$ncount <- data$count / max(abs(data$count), na.rm = TRUE)
      if (drop) data <- subset(data, count > 0)
    }
  }


    if (binaxis == "x") {
      names(data)[names(data) == "bincenter"] <- "x"
      # For x binning, the width of the geoms is same as the width of the bin
      data$width <- data$binwidth
    } else if (binaxis == "y") {
      names(data)[names(data) == "bincenter"] <- "y"
    }
    return(data)
  }

  icon <- function(.) GeomDotplot$icon()
  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomDotplot
  
})

# This does density binning, but does not collapse each bin with a count.
# It returns a data frame with the original data (x), weights, bin #, and the bin centers.
densitybin <- function(x, weight = NULL, binwidth = NULL, binmethod = binmethod, range = NULL) {

    if (length(na.omit(x)) == 0) return(data.frame())
    if (is.null(weight))  weight <- rep(1, length(x))
    weight[is.na(weight)] <- 0

    if (is.null(range))    range <- range(x, na.rm = TRUE, finite = TRUE)
    if (is.null(binwidth)) binwidth <- diff(range) / 30

    # Sort weight and x, by x
    weight <- weight[order(x)]
    x      <- x[order(x)]

    cbin    <- 0                      # Current bin ID
    bin     <- rep.int(NA, length(x)) # The bin ID for each observation
    binend  <- -Inf                   # End position of current bin (scan left to right)

    # Scan list and put dots in bins
    for (i in 1:length(x)) {
        # If past end of bin, start a new bin at this point
        if (x[i] >= binend) {
            binend <- x[i] + binwidth
            cbin <- cbin + 1
        }

        bin[i] <- cbin
    }

    results <- data.frame(x, bin, binwidth, weight)
    results <- ddply(results, .(bin), function(df) {
                    df$bincenter = (min(df$x) + max(df$x)) / 2
                    return(df)
                  })

    return(results)
}
