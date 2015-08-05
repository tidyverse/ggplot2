#' Blank, draws nothing.
#'
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots.
#'
#' @export
#' @inheritParams geom_point
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) + geom_blank()
#' # Nothing to see here!
#'
#' # Take the following scatter plot
#' a <- ggplot(mtcars, aes(x = wt, y = mpg), . ~ cyl) + geom_point()
#' # Add to that some lines with geom_abline()
#' df <- data.frame(a = rnorm(10, 25), b = rnorm(10, 0))
#' a + geom_abline(aes(intercept = a, slope = b), data = df)
#' # Suppose you then wanted to remove the geom_point layer
#' # If you just remove geom_point, you will get an error
#' b <- ggplot(mtcars, aes(x = wt, y = mpg))
#' \dontrun{b + geom_abline(aes(intercept = a, slope = b), data = df)}
#' # Switching to geom_blank() gets the desired plot
#' c <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank()
#' c + geom_abline(aes(intercept = a, slope = b), data = df)
geom_blank <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", show.legend = NA,
                       inherit.aes = FALSE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBlank,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBlank <- ggproto("GeomBlank", Geom,
  default_aes = aes(),
  draw = function(...) nullGrob()
)
