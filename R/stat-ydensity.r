#' 1d kernel density estimate along y axis, for violin plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "ydensity")}
#'
#' @inheritParams stat_density
#' @inheritParams stat_identity
#' @param trim If \code{TRUE} (default), trim the tails of the violins
#'   to the range of the data. If \code{FALSE}, don't trim the tails.
#' @param scale if "area" (default), all violins have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#'
#' @return A data frame with additional columns:
#'   \item{density}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points - probably useless for violin plots}
#'   \item{violinwidth}{density scaled for the violin plot, according to area, counts
#'                      or to a constant maximum width}
#'   \item{n}{number of points}
#'   \item{width}{width of violin bounding box}
#' @seealso \code{\link{geom_violin}} for examples, and \code{\link{stat_density}}
#'   for examples with data along the x axis.
#' @export
#' @examples
#' # See geom_violin for examples
#' # Also see stat_density for similar examples with data along x axis
stat_ydensity <- function (mapping = NULL, data = NULL, geom = "violin", position = "dodge",
adjust = 1, kernel = "gaussian", trim = TRUE, scale = "area", na.rm = FALSE, ...) {
  StatYdensity$new(mapping = mapping, data = data, geom = geom, position = position,
  adjust = adjust, kernel = kernel, trim = trim, scale = scale,
  na.rm = na.rm, ...)
}

StatYdensity <- proto(Stat, {
  objname <- "ydensity"

  calculate_groups <- function(., data, na.rm = FALSE, width = NULL,
                               scale = "area", ...) {
    data <- remove_missing(data, na.rm, "y", name = "stat_ydensity", finite = TRUE)
    data <- .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)

    # choose how violins are scaled relative to each other
    scale <- match.arg(scale, c("area", "equal", "count", "width"))
    if (scale == "equal") {
      gg_dep("0.9.2", "scale=\"area\" is deprecated; in the future, use scale=\"equal\" instead.")
      scale <- "area"
    }

    data$violinwidth <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = (data$density / max(data$density)) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )

    data
  }

  calculate <- function(., data, scales, width=NULL, adjust=1, kernel="gaussian",
                        trim=TRUE, na.rm = FALSE, ...) {

    n <- nrow(data)

    # if less than 3 points, return a density of 1 everywhere
    if (n < 3) {
      return(data.frame(data, density = 1, scaled = 1, count = 1))
    }

    # initialize weights if they are not supplied by the user
    if (is.null(data$weight)) { data$weight <- rep(1, n) / n }

    # compute the density
    dens <- density(data$y, adjust = adjust, kernel = kernel,
      weight = data$weight, n = 200)

    # NB: stat_density restricts to the scale range, here we leave that
    # free so violins can extend the y scale
    densdf <- data.frame(y = dens$x, density = dens$y)

    # scale density to a maximum of 1
    densdf$scaled <- densdf$density / max(densdf$density, na.rm = TRUE)

    # trim density outside of the data range
    if (trim) {
      densdf <- subset(densdf, y > min(data$y, na.rm = TRUE) & y < max(data$y, na.rm = TRUE))
    }
    # NB: equivalently, we could have used these bounds in the from and
    # to arguments of density()

    # scale density by the number of observations
    densdf$count <- densdf$density * n
    # record the number of observations to be able to scale the density later
    densdf$n <- n

    # coordinate on the x axis
    densdf$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))

    # width of the bounding box of the violin plot on the x axis for continuous x
    if (length(unique(data$x)) > 1) { width <- diff(range(data$x)) * 0.9 }
    densdf$width <- width

    densdf
  }

  default_geom <- function(.) GeomViolin
  required_aes <- c("x", "y")

})
