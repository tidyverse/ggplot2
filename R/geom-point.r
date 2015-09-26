#' Points, as for a scatterplot
#'
#' The point geom is used to create scatterplots.
#'
#' The scatterplot is useful for displaying the relationship between two
#' continuous variables, although it can also be used with one continuous
#' and one categorical variable, or two categorical variables.  See
#' \code{\link{geom_jitter}} for possibilities.
#'
#' The \emph{bubblechart} is a scatterplot with a third variable mapped to
#' the size of points.  There are no special names for scatterplots where
#' another variable is mapped to point shape or colour, however.
#'
#' The biggest potential problem with a scatterplot is overplotting: whenever
#' you have more than a few points, points may be plotted on top of one
#' another. This can severely distort the visual appearance of the plot.
#' There is no one solution to this problem, but there are some techniques
#' that can help.  You can add additional information with
#' \code{\link{stat_smooth}}, \code{\link{stat_quantile}} or
#' \code{\link{stat_density2d}}.  If you have few unique x values,
#' \code{\link{geom_boxplot}} may also be useful.  Alternatively, you can
#' summarise the number of points at each location and display that in some
#' way, using \code{\link{stat_sum}}. Another technique is to use transparent
#' points, e.g. \code{geom_point(alpha = 0.05)}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @seealso \code{\link{scale_size}} to see scale area of points, instead of
#'   radius, \code{\link{geom_jitter}} to jitter points to reduce (mild)
#'   overplotting
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. There are
#'   three types of arguments you can use here:
#'
#'   \itemize{
#'   \item Aesthetics: to set an aesthetic to a fixed value, like
#'      \code{color = "red"} or \code{size = 3}.
#'   \item Other arguments to the layer, for example you override the
#'     default \code{stat} associated with the layer.
#'   \item Other arguments passed on to the stat.
#'   }
#' @inheritParams layer
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point()
#'
#' # Add aesthetic mappings
#' p + geom_point(aes(colour = factor(cyl)))
#' p + geom_point(aes(shape = factor(cyl)))
#' p + geom_point(aes(size = qsec))
#'
#' # Change scales
#' p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
#' p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
#'
#' # Set aesthetics to fixed value
#' ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red", size = 3)
#'
#' \donttest{
#' # Varying alpha is useful for large datasets
#' d <- ggplot(diamonds, aes(carat, price))
#' d + geom_point(alpha = 1/10)
#' d + geom_point(alpha = 1/20)
#' d + geom_point(alpha = 1/100)
#' }
#'
#' # For shapes that have a border (like 21), you can colour the inside and
#' # outside separately. Use the stroke aesthetic to modify the width of the
#' # border
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)
#'
#' \donttest{
#' # You can create interesting shapes by layering multiple points of
#' # different sizes
#' p <- ggplot(mtcars, aes(mpg, wt, shape = factor(cyl)))
#' p + geom_point(aes(colour = factor(cyl)), size = 4) +
#'   geom_point(colour = "grey90", size = 1.5)
#' p + geom_point(colour = "black", size = 4.5) +
#'   geom_point(colour = "pink", size = 4) +
#'   geom_point(aes(shape = factor(cyl)))
#'
#' # These extra layers don't usually appear in the legend, but we can
#' # force their inclusion
#' p + geom_point(colour = "black", size = 4.5, show.legend = TRUE) +
#'   geom_point(colour = "pink", size = 4, show.legend = TRUE) +
#'   geom_point(aes(shape = factor(cyl)))
#'
#' # geom_point warns when missing values have been dropped from the data set
#' # and not plotted, you can turn this off by setting na.rm = TRUE
#' mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
#' ggplot(mtcars2, aes(wt, mpg)) + geom_point()
#' ggplot(mtcars2, aes(wt, mpg)) + geom_point(na.rm = TRUE)
#' }
geom_point <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPoint <- ggproto("GeomPoint", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    ggname("geom_point",
      pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          # Stroke is added around the outside of the point
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
    )
  },

  draw_key = draw_key_point
)
