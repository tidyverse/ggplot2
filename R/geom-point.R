#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomPoint <- ggproto(
  "GeomPoint", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = from_theme(pointshape),
    colour = from_theme(colour %||% ink),
    fill = from_theme(fill %||% NA),
    size = from_theme(pointsize),
    alpha = NA,
    stroke = from_theme(borderwidth)
  ),

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    data$shape <- translate_shape_string(data$shape)
    coords <- coord$transform(data, panel_params)
    ggname(
      "geom_point",
      pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = gg_par(
          col = alpha(coords$colour, coords$alpha),
          fill = fill_alpha(coords$fill, coords$alpha),
          pointsize = coords$size,
          stroke = coords$stroke
        )
      )
    )
  },

  draw_key = draw_key_point
)

#' Points
#'
#' The point geom is used to create scatterplots. The scatterplot is most
#' useful for displaying the relationship between two continuous variables.
#' It can be used to compare one continuous and one categorical variable, or
#' two categorical variables, but a variation like [geom_jitter()],
#' [geom_count()], or [geom_bin_2d()] is usually more
#' appropriate. A _bubblechart_ is a scatterplot with a third variable
#' mapped to the size of points.
#'
#' @section Overplotting:
#' The biggest potential problem with a scatterplot is overplotting: whenever
#' you have more than a few points, points may be plotted on top of one
#' another. This can severely distort the visual appearance of the plot.
#' There is no one solution to this problem, but there are some techniques
#' that can help. You can add additional information with
#' [geom_smooth()], [geom_quantile()] or
#' [geom_density_2d()]. If you have few unique `x` values,
#' [geom_boxplot()] may also be useful.
#'
#' Alternatively, you can
#' summarise the number of points at each location and display that in some
#' way, using [geom_count()], [geom_hex()], or
#' [geom_density2d()].
#'
#' Another technique is to make the points transparent (e.g.
#' `geom_point(alpha = 0.05)`) or very small (e.g.
#' `geom_point(shape = ".")`).
#'
#' @aesthetics GeomPoint
#' The `fill` aesthetic only applies to shapes 21-25.
#' @inheritParams shared_layer_parameters
#'
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point()
#'
#' # Add aesthetic mappings
#' p + geom_point(aes(colour = factor(cyl)))
#' p + geom_point(aes(shape = factor(cyl)))
#' # A "bubblechart":
#' p + geom_point(aes(size = qsec))
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
#' # The default shape in legends is not filled, but you can override the shape
#' # in the guide to reflect the fill in the legend
#' ggplot(mtcars, aes(wt, mpg, fill = factor(carb), shape = factor(cyl))) +
#'   geom_point(size = 5, stroke = 1) +
#'   scale_shape_manual(values = 21:25) +
#'   scale_fill_ordinal(guide = guide_legend(override.aes = list(shape = 21)))
#'
#' \donttest{
#' # You can create interesting shapes by layering multiple points of
#' # different sizes
#' p <- ggplot(mtcars, aes(mpg, wt, shape = factor(cyl)))
#' p +
#'   geom_point(aes(colour = factor(cyl)), size = 4) +
#'   geom_point(colour = "grey90", size = 1.5)
#' p +
#'   geom_point(colour = "black", size = 4.5) +
#'   geom_point(colour = "pink", size = 4) +
#'   geom_point(aes(shape = factor(cyl)))
#'
#' # geom_point warns when missing values have been dropped from the data set
#' # and not plotted, you can turn this off by setting na.rm = TRUE
#' set.seed(1)
#' mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
#' ggplot(mtcars2, aes(wt, mpg)) +
#'   geom_point()
#' ggplot(mtcars2, aes(wt, mpg)) +
#'   geom_point(na.rm = TRUE)
#' }
geom_point <- make_constructor(GeomPoint)

#' Translating shape strings
#'
#' `translate_shape_string()` is a helper function for translating point shapes
#' given as a character vector into integers that are interpreted by the
#' grid system.
#'
#' @param shape_string A character vector giving point shapes. Non-character
#'   input will be returned.
#'
#' @return An integer vector with translated shapes.
#' @export
#' @keywords internal
#'
#' @examples
#' translate_shape_string(c("circle", "square", "triangle"))
#'
#' # Strings with 1 or less characters are interpreted as symbols
#' translate_shape_string(c("a", "b", "?"))
translate_shape_string <- function(shape_string) {
  if (!is.character(shape_string)) {
    return(shape_string)
  }
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique0(shape_string[invalid_strings])
    cli::cli_abort("Shape aesthetic contains invalid value{?s}: {.val {bad_string}}.")
  }

  if (any(nonunique_strings)) {
    bad_string <- unique0(shape_string[nonunique_strings])
    cli::cli_abort(c(
      "Shape names must be given unambiguously.",
      "i" = "Fix {.val {bad_string}}."
    ))
  }

  unname(pch_table[shape_match])
}
