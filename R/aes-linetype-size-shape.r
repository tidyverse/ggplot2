#' Differentiation related aesthetics: linetype, size, shape
#'
#' @description
#' This page demonstrates the usage of a sub-group
#' of aesthetics to further customize ggplot: `linetype`, `size` and `shape`.
#'
#' @details
#' `linetype` can be specified with either an integer (0-6), a name (0 = blank, 1 = solid,
#' 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash), a
#' variable source (for grouping) or a string of
#' an even number (up to eight) of hexadecimal digits which give the lengths in
#' consecutive positions in the string. See example for sample hex string.
#' `size` can be specified with a numerical value (in millimetres),
#' or from a variable source (for grouping).
#' `shape` can be specified with: an integer (between 0 and 25),
#' a single character (which uses that character as the plotting symbol),
#' a `.` to draw the smallest rectangle that is visible (i.e., about one pixel),
#' an `NA` to draw nothing, or a variable source (for grouping). Symbols and filled shapes are
#' described in the examples below.
#'
#' @seealso
#' [aes_group_order()] for using `linetype`, `size`, and `shape` for grouping.
#'
#' [geom_line()] and [geom_point()] for geometries that are commonly used with these aesthetics.
#'
#' Learn more about aesthetics in `vignette("ggplot2-specs")`.
#'
#' @name aes_linetype_size_shape
#' @aliases linetype size shape
#' @examples
#'
#' df <- data.frame(x = 1:10 , y = 1:10)
#' f <- ggplot(df, aes(x, y))
#' f + geom_line(linetype = 2)
#' f + geom_line(linetype = "dotdash")
#'
#' # An example with hex strings, the string "33" specifies three units on followed
#' # by three off and "3313" specifies three units on followed by three off followed
#' # by one on and finally three off.
#' f + geom_line(linetype = "3313")
#'
#' # Mapping line type from a grouping variable
#' ggplot(economics_long, aes(date, value01)) +
#'   geom_line(aes(linetype = variable))
#'
#' # Size examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point(size = 4)
#' p + geom_point(aes(size = qsec))
#' p + geom_point(size = 2.5) +
#'   geom_hline(yintercept = 25, size = 3.5)
#'
#' # Shape examples
#' p + geom_point()
#' p + geom_point(shape = 5)
#' p + geom_point(shape = "k", size = 3)
#' p + geom_point(shape = ".")
#' p + geom_point(shape = NA)
#' p + geom_point(aes(shape = factor(cyl)))
#'
#' # A look at all 25 symbols
#' df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
#' s <- ggplot(df2, aes(x, y))
#' s + geom_point(aes(shape = z), size = 4) +
#'   scale_shape_identity()
#' # While all symbols have a foreground colour, symbols 19-25 also take a
#' # background colour (fill)
#' s + geom_point(aes(shape = z), size = 4, colour = "Red") +
#'   scale_shape_identity()
#' s + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
#'   scale_shape_identity()
NULL
