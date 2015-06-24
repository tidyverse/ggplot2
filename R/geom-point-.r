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
#' points, \code{geom_point(alpha = 0.05)}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @seealso \code{\link{scale_size}} to see scale area of points, instead of
#'   radius, \code{\link{geom_jitter}} to jitter points to reduce (mild)
#'   overplotting
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param position Postion adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param show_guide logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
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
#' \donttest{
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point()
#'
#' # Add aesthetic mappings
#' p + geom_point(aes(colour = qsec))
#' p + geom_point(aes(alpha = qsec))
#' p + geom_point(aes(colour = factor(cyl)))
#' p + geom_point(aes(shape = factor(cyl)))
#' p + geom_point(aes(size = qsec))
#'
#' # Change scales
#' p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
#' p + geom_point(aes(size = qsec)) + scale_size_area()
#' p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
#'
#' # Set aesthetics to fixed value
#' p + geom_point(colour = "red", size = 3)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red", size = 3)
#'
#' # Varying alpha is useful for large datasets
#' d <- ggplot(diamonds, aes(carat, price))
#' d + geom_point(alpha = 1/10)
#' d + geom_point(alpha = 1/20)
#' d + geom_point(alpha = 1/100)
#'
#' # For shapes that have a border (like 21), you can colour the inside and
#' # outside separately. Use the stroke aesthetic to modify the width of the
#' # border
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(shape = 21, size = 5, colour = "black", fill = "white", stroke = 5)
#'
#' # You can create interesting shapes by layering multiple points of
#' # different sizes
#' p <- ggplot(mtcars, aes(mpg, wt))
#' p + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = cyl))
#' p + aes(shape = factor(cyl)) +
#'   geom_point(aes(colour = factor(cyl)), size = 4) +
#'   geom_point(colour="grey90", size = 1.5)
#' p + geom_point(colour="black", size = 4.5) +
#'   geom_point(colour="pink", size = 4) +
#'   geom_point(aes(shape = factor(cyl)))
#'
#' # These extra layers don't usually appear in the legend, but we can
#' # force their inclusion
#' p + geom_point(colour="black", size = 4.5, show_guide = TRUE) +
#'   geom_point(colour="pink", size = 4, show_guide = TRUE) +
#'   geom_point(aes(shape = factor(cyl)))
#'
#' # Transparent points:
#' ggplot(mtcars, aes(mpg, wt)) + geom_point(size = 5, alpha = 1/5)
#'
#' # geom_point warns when missing values have been dropped from the data set
#' # and not plotted, you can turn this off by setting na.rm = TRUE
#' mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
#' ggplot(mtcars2, aes(wt, mpg)) + geom_point()
#' ggplot(mtcars2, aes(wt, mpg)) + geom_point(na.rm = TRUE)
#' }
geom_point <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
na.rm = FALSE, show_guide = NA, ...) {
  GeomPoint$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, show_guide = show_guide, ...)
}

GeomPoint <- proto(Geom, {
  objname <- "point"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "size", "shape"), name = "geom_point")
    if (empty(data)) return(zeroGrob())

    with(coord_transform(coordinates, data, scales),
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), lwd = stroke, fontsize = size * .pt)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
      gp=gpar(
        col=alpha(colour, alpha),
        fill=alpha(fill, alpha),
        lwd=stroke,
        fontsize = size * .pt),
      )
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=19, colour="black", size=2, fill = NA, alpha = NA, stroke = 1)

})


#' @export
geom_point2 <- function (mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, show_guide = NA, ...) {
  LayerR6$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointR6,
    position = position,
    show_guide = show_guide,
    params = list(na.rm = na.rm, ...)
  )
}

GeomPointR6 <- R6::R6Class("GeomPointR6", inherit = GeomR6,
  public = list(
    objname = "point",

    draw_groups = function(...) self$draw(...),

    draw = function(data, scales, coordinates, na.rm = FALSE, ...) {
      data <- remove_missing(data, na.rm,
        c("x", "y", "size", "shape"), name = "geom_point")
      if (empty(data)) return(zeroGrob())

      with(coord_transform(coordinates, data, scales),
        ggname(self$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
        gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), lwd = stroke, fontsize = size * .pt)))
      )
    },

    draw_legend = function(data, ...) {
      data <- aesdefaults(data, self$default_aes(), list(...))

      with(data,
        pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
        gp=gpar(
          col=alpha(colour, alpha),
          fill=alpha(fill, alpha),
          lwd=stroke,
          fontsize = size * .pt)
        )
      )
    },

    default_stat = function() StatIdentity,
    required_aes = c("x", "y"),
    default_aes = function() aes(shape=19, colour="black", size=2, fill = NA, alpha = NA, stroke = 1)
  )
)
