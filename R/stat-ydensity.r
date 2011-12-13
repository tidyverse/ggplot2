#' 1d kernel density estimate along y axis, for violin plot.
#'
#' @return A data frame with additional columns:
#'   \item{width}{width of violin bounding box}
#'   \item{vdensity}{density estimate}
#'   \item{scaled}{density estimate, scaled depending on scalearea and scalecount}
#'   \item{count}{density * number of points} 
#'   \item{counttotal}{number of points} 
#' @examples
#' # See geom_violin for examples
#' # Also see stat_density for similar examples with data along x axis
stat_ydensity <- function (mapping = NULL, data = NULL, geom = "violin", position = "dodge", 
adjust = 1, kernel = "gaussian", trim = TRUE, scalearea = FALSE, scalecount = FALSE, na.rm = FALSE, ...) {
  StatYdensity$new(mapping = mapping, data = data, geom = geom, position = position,
  adjust = adjust, kernel = kernel, trim = trim, scalearea = scalearea, scalecount = scalecount,
  na.rm = na.rm, ...)
}
  
StatYdensity <- proto(Stat, {
  objname <- "ydensity"

  calculate_groups <- function(., data, na.rm = FALSE, width = NULL,
                               scalearea = FALSE, scalecount = FALSE, ...) {
    data <- remove_missing(data, na.rm, "y", name = "stat_ydensity", finite = TRUE)
    data <- .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)

    if (scalearea)
      data$scaled <- data$ydensity / max(data$ydensity)

    if (scalecount)
      data$scaled <- data$scaled * data$counttotal/max(data$counttotal)

    data
  }

  calculate <- function(., data, scales, width=NULL, adjust=1, kernel="gaussian",
                        trim=TRUE, na.rm = FALSE, ...) {
    
    n <- nrow(data)

    # If just 1 point, return a flat violin
    if (n < 2) return(data.frame(data, scaled=1, ydensity=1, count=1))
    if (is.null(data$weight)) data$weight <- rep(1, n) / n

    if(trim) 
      dens <- density(data$y, adjust=adjust, kernel=kernel, weight=data$weight, n=200,
                      from=min(data$y), to=max(data$y))
    else 
      dens <- density(data$y, adjust=adjust, kernel=kernel, weight=data$weight, n=200)

    # We predict ydensity from y ('density' calls them y and x, respectively)
    densdf <- data.frame(ydensity=dens$y, y=dens$x)
    densdf$scaled <- densdf$ydensity / max(densdf$ydensity, na.rm = TRUE)

    if (length(unique(data$x)) > 1) width <- diff(range(data$x)) * 0.9

    densdf$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    densdf$count <- densdf$ydensity * n
    densdf$counttotal <- n
    densdf$width <- width

    densdf
  }

  icon <- function(.) GeomViolin$icon()
  default_geom <- function(.) GeomViolin
  required_aes <- c("x", "y")

})
