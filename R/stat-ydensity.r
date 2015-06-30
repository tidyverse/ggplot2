#' 1d kernel density estimate along y axis, for violin plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "ydensity")}
#'
#' @inheritParams stat_density
#' @inheritParams stat_identity
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
stat_ydensity <- function (mapping = NULL, data = NULL, geom = "violin",
  position = "dodge", adjust = 1, kernel = "gaussian", trim = TRUE,
  scale = "area", na.rm = FALSE, show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatYdensity,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      na.rm = na.rm
    ),
    params = list(...)
  )
}


StatYdensity <- proto2(
  inherit = Stat,
  members = list(
    objname = "ydensity",

    calculate_groups = function(self, data, na.rm = FALSE, width = NULL,
      scale = "area", ...)
    {
      data <- remove_missing(data, na.rm, c("x", "y", "weight"), name = "stat_ydensity", finite = TRUE)
      data <- self$super$calculate_groups(self, data, na.rm = na.rm, width = width, ...)

      # choose how violins are scaled relative to each other
      scale <- match.arg(scale, c("area", "count", "width"))

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
    },

    calculate = function(self, data, scales, width = NULL, adjust = 1,
      kernel = "gaussian", trim = FALSE, na.rm = FALSE, ...)
    {
      data <- remove_missing(data, na.rm, "x", name = "stat_density",
        finite = TRUE)

      if (trim) {
        range <- range(data$y, na.rm = TRUE)
      } else {
        range <- scale_dimension(scales$y, c(0, 0))
      }
      dens <- compute_density(data$y, data$w, from = range[1], to = range[2],
        adjust = adjust, kernel = kernel)

      dens$y <- dens$x
      dens$x <- mean(range(data$x))

      # Compute width if x has multiple values
      if (length(unique(data$x)) > 1) {
        width <- diff(range(data$x)) * 0.9
      }
      dens$width <- width
      dens
    },

    default_geom = function(self) GeomViolin,

    required_aes = c("x", "y")
  )
)
