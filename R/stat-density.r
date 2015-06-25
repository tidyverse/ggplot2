#' 1d kernel density estimate.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "density")}
#'
#' @param adjust see \code{\link{density}} for details
#' @param kernel kernel used for density estimation, see
#'   \code{\link{density}} for details
#' @param trim This parameter only matters if you are displaying multiple
#'   densities in one plot. If \code{FALSE}, the default, each density is
#'   computed on the full range of the data. If \code{TRUE}, each density
#'   is computed over the range of that group: this typically means the
#'   estimated x values will not line-up, and hence you won't be able to
#'   stack density values.
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
#' # Make a violin plot
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
#' ggplot(movies, aes(rating, ..density..)) +
#'   geom_density(position = "stack", aes(fill = mpaa))
#' # Preserves marginal densities
#' ggplot(movies, aes(rating, ..count..)) +
#'   geom_density(position = "stack", aes(fill = mpaa))
#'
#' # You can use position="fill" to produce a conditional density estimate
#' ggplot(movies, aes(rating, ..count..)) +
#'   geom_density(position = "fill", aes(fill = mpaa))
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
#' }
stat_density <- function (mapping = NULL, data = NULL, geom = "area",
  position = "stack", adjust = 1, kernel = "gaussian", trim = FALSE,
  na.rm = FALSE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatDensity,
    geom = geom,
    position = position,
    params = list(
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      na.rm = na.rm,
      ...
    )
  )
}

StatDensity <- R6::R6Class("StatDensity", inherit = Stat,
  public = list(
    objname = "density",

    calculate = function(data, scales, adjust = 1, kernel = "gaussian",
      trim = FALSE, na.rm = FALSE, ...)
    {
      data <- remove_missing(data, na.rm, "x", name = "stat_density",
        finite = TRUE)

      if (trim) {
        range <- range(data$x, na.rm = TRUE)
      } else {
        range <- scale_dimension(scales$x, c(0, 0))
      }

      compute_density(data$x, data$w, from = range[1], to = range[2],
        adjust = adjust, kernel = kernel)
    },

    default_geom = function() GeomArea,

    default_aes = function() aes(y = ..density.., fill = NA),

    required_aes = c("x")
  )
)

compute_density <- function(x, w, from, to, bw = "nrd0", adjust = 1,
                            kernel = "gaussian") {
  n <- length(x)
  if (is.null(w)) {
    w <- rep(1 / n, n)
  }

  # if less than 3 points, spread density evenly over points
  if (n < 3) {
    return(data.frame(
      x = x,
      density = w / sum(w),
      scaled = w / max(w),
      count = 1,
      n = n
    ))
  }

  dens <- stats::density(x, weight = w, bw = bw, adjust = adjust,
    kernel = kernel, from = from, to = to)

  data.frame(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = TRUE),
    count =   dens$y * n,
    n = n
  )
}
