#' 1d kernel density estimate along y axis, for violin plot.
#'
#' @return A data frame with additional columns:
#'   \item{width}{width of violin bounding box}
#'   \item{vdensity}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points} 
#' @examples
#' # See geom_violin for examples
#' # Also see stat_density for similar examples with data along x axis
stat_ydensity <- function (mapping = NULL, data = NULL, geom = "violin", position = "dodge", 
adjust = 1, kernel = "gaussian", trim = TRUE, fullwidth = TRUE, na.rm = FALSE, ...) { 
  StatYdensity$new(mapping = mapping, data = data, geom = geom, position = position,
  adjust = adjust, kernel = kernel, trim = trim, fullwidth = fullwidth, na.rm = na.rm, ...)
}
  
StatYdensity <- proto(Stat, {
  objname <- "ydensity"

  calculate_groups <- function(., data, na.rm = FALSE, width = NULL, fullwidth=TRUE, ...) {
    data <- remove_missing(data, na.rm, "y", name = "stat_ydensity", finite = TRUE)
    data <- .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)

    # If !fullwidth, scale so that the widest violin has scaled=1, and others
    #  are proportionally narrower, based on their density curves.
    if (!fullwidth) {
      data$scaled <- data$ydensity / max(data$ydensity)
    }
    data
  }

  calculate <- function(., data, scales, width=NULL, adjust=1, kernel="gaussian",
                        trim=TRUE, na.rm = FALSE, ...) {
    
    n <- nrow(data)

    # If just 1 point, return a flat violin
    if (n < 2) return(data.frame(data, scaled=1, ydensity=1, count=1))
    if (is.null(data$weight)) data$weight <- rep(1, n) / n

    if(trim) 
      dens <- density(data$y, adjust=adjust, kernel=kernel,
                      from=min(data$y), to=max(data$y), weight=data$weight)
    else 
      dens <- density(data$y, adjust=adjust, kernel=kernel, weight=data$weight)

    densdf <- data.frame(x=dens$y, y=dens$x) # Do a swap on x and y
    densdf$scaled <- densdf$x / max(densdf$x, na.rm = TRUE)

    if (length(unique(data$x)) > 1) width <- diff(range(data$x)) * 0.9

    within(densdf, {
      ydensity <- x
      x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
      count <- x * n
      width <- width
    })

  }

  icon <- function(.) GeomViolin$icon()
  default_geom <- function(.) GeomViolin
  required_aes <- c("x", "y")

})
