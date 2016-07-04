#' Count the number of observations at each location.
#'
#' This is a variant \code{\link{geom_point}} that counts the number of
#' observations at each location, then maps the count to point size. It
#' useful when you have discrete data.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#' @param geom,stat Use to override the default connection between
#'   \code{geom_count} and \code{stat_sum}.
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_point()
#'
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_count()
#'
#' # Best used in conjunction with scale_size_area which ensures that
#' # counts of zero would be given size 0. Doesn't make much different
#' # here because the smallest count is already close to 0.
#' ggplot(mpg, aes(cty, hwy)) +
#'  geom_count()
#'  scale_size_area()
#'
#' # Display proportions instead of counts -------------------------------------
#' # By default, all categorical variables in the plot form the groups.
#' # Specifying geom_count without a group identifier leads to a plot which is
#' # not useful:
#' d <- ggplot(diamonds, aes(x = cut, y = clarity))
#' d + geom_count(aes(size = ..prop..))
#' # To correct this problem and achieve a more desirable plot, we need
#' # to specify which group the proportion is to be calculated over.
#' d + geom_count(aes(size = ..prop.., group = 1)) +
#'   scale_size_area(max_size = 10)
#'
#' # Or group by x/y variables to have rows/columns sum to 1.
#' d + geom_count(aes(size = ..prop.., group = cut)) +
#'   scale_size_area(max_size = 10)
#' d + geom_count(aes(size = ..prop.., group = clarity)) +
#'   scale_size_area(max_size = 10)
geom_count <- function(mapping = NULL, data = NULL,
                       stat = "sum", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
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
