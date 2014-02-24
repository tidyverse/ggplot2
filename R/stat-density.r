#' 1d kernel density estimate.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "density")}
#'
#' @param adjust see \code{\link{density}} for details
#' @param kernel kernel used for density estimation, see
#'   \code{\link{density}} for details
#' @param trim if \code{TRUE}, the default, densities are trimmed to the
#'   actual range of the data.  If \code{FALSE}, they are extended by the
#'   default 3 bandwidths (as specified by the \code{cut} parameter to
#'   \code{\link{density}})
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#' @return data.frame with additional columns:
#'   \item{density}{density estimate}
#'   \item{count}{density * number of points - useful for stacked density
#'      plots}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#' @seealso \code{\link{stat_bin}} for the histogram
#' @export
#' @examples
#' \donttest{
#' m <- ggplot(movies, aes(x = rating))
#' m + geom_density()
#'
#' # Adjust parameters
#' m + geom_density(kernel = "rectangular")
#' m + geom_density(kernel = "biweight")
#' m + geom_density(kernel = "epanechnikov")
#' m + geom_density(adjust=1/5) # Very rough
#' m + geom_density(adjust=5) # Very smooth
#'
#' # Adjust aesthetics
#' m + geom_density(aes(fill=factor(Drama)), size=2)
#' # Scale so peaks have same height:
#' m + geom_density(aes(fill=factor(Drama), y = ..scaled..), size=2)
#'
#' m + geom_density(colour="darkgreen", size=2)
#' m + geom_density(colour="darkgreen", size=2, fill=NA)
#' m + geom_density(colour="darkgreen", size=2, fill="green")
#'
#' # Change scales
#' (m <- ggplot(movies, aes(x=votes)) + geom_density(trim = TRUE))
#' m + scale_x_log10()
#' m + coord_trans(x="log10")
#' m + scale_x_log10() + coord_trans(x="log10")
#'
#' # Also useful with
#' m + stat_bin()
#'
#' # Make a volcano plot
#' ggplot(diamonds, aes(x = price)) +
#'   stat_density(aes(ymax = ..density..,  ymin = -..density..),
#'     fill = "grey50", colour = "grey50",
#'     geom = "ribbon", position = "identity") +
#'   facet_grid(. ~ cut) +
#'   coord_flip()
#'
#' # Stacked density plots
#' # If you want to create a stacked density plot, you need to use
#' # the 'count' (density * n) variable instead of the default density
#'
#' # Loses marginal densities
#' qplot(rating, ..density.., data=movies, geom="density", fill=mpaa, position="stack")
#' # Preserves marginal densities
#' qplot(rating, ..count.., data=movies, geom="density", fill=mpaa, position="stack")
#'
#' # You can use position="fill" to produce a conditional density estimate
#' qplot(rating, ..count.., data=movies, geom="density", fill=mpaa, position="fill")
#'
#' # Need to be careful with weighted data
#' m <- ggplot(movies, aes(x=rating, weight=votes))
#' m + geom_histogram(aes(y = ..count..)) + geom_density(fill=NA)
#'
#' m <- ggplot(movies, aes(x=rating, weight=votes/sum(votes)))
#' m + geom_histogram(aes(y=..density..)) + geom_density(fill=NA, colour="black")
#'
#' library(plyr) # to access round_any
#' movies$decade <- round_any(movies$year, 10)
#' m <- ggplot(movies, aes(x=rating, colour=decade, group=decade))
#' m + geom_density(fill=NA)
#' m + geom_density(fill=NA) + aes(y = ..count..)
#'
#' # Use qplot instead
#' qplot(length, data=movies, geom="density", weight=rating)
#' qplot(length, data=movies, geom="density", weight=rating/sum(rating))
#' }
stat_density <- function (mapping = NULL, data = NULL, geom = "area", position = "stack",
adjust = 1, kernel = "gaussian", trim = FALSE, na.rm = FALSE, ...) {
  StatDensity$new(mapping = mapping, data = data, geom = geom, position = position,
  adjust = adjust, kernel = kernel, trim = trim, na.rm = na.rm, ...)
}

StatDensity <- proto(Stat, {
  objname <- "density"

  calculate <- function(., data, scales, adjust=1, kernel="gaussian", trim=FALSE, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, "x", name = "stat_density",
      finite = TRUE)

    n <- nrow(data)
    if (n < 3) return(data.frame())
    if (is.null(data$weight)) data$weight <- rep(1, n) / n

    range <- scale_dimension(scales$x, c(0, 0))
    xgrid <- seq(range[1], range[2], length=200)

    dens <- density(data$x, adjust=adjust, kernel=kernel, weight=data$weight, from=range[1], to=range[2])
    densdf <- as.data.frame(dens[c("x","y")])

    densdf$scaled <- densdf$y / max(densdf$y, na.rm = TRUE)
    if (trim) densdf <- subset(densdf, x > min(data$x, na.rm = TRUE) & x < max(data$x, na.rm = TRUE))

    densdf$count <- densdf$y * n
    rename(densdf, c(y = "density"), warn_missing = FALSE)
  }

  default_geom <- function(.) GeomArea
  default_aes <- function(.) aes(y = ..density.., fill=NA)
  required_aes <- c("x")

})
