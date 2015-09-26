#' Display a smoke plot.
#'
#' A smoke plot is several superimposed kernel density estimates,
#' each with a low transparency.  They are  useful for displaying
#' the distribution of variables with small and large scale
#' structure.
#'
#' @section Aesthetics:
#'   \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
#'   "density")}
#'
#' @seealso See \code{\link{geom_histogram}},
#'   \code{\link{geom_freqpoly}}, and \code{\link{geom_density}} for
#'   other methods of displaying continuous distribution. See
#'   \code{\link{geom_violin}} for a compact density display.
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_smoke} and \code{stat_smoke}.
#' @export
#' @examples
#'
#' theme_set(theme_bw())
#'
#' ggplot(diamonds, aes(carat)) +
#'   geom_smoke()
#'
#' last_plot() + geom_density()
#'
#' # smoke2 is the same here, but slower
#' # we'll see why it's helpful below
#' ggplot(diamonds, aes(carat)) +
#'   geom_smoke2()
#'
#'
#' set.seed(10)
#' x <- c(rnorm(200), rnorm(30, 2, .1))
#' qplot(x, geom = "density")
#' qplot(x, geom = "smoke")
#' qplot(x, geom = "histogram")
#'
#'
#' set.seed(1)
#' x <- c(rnorm(200), rnorm(50, 0, .025))
#' qplot(x, geom = "density")
#' qplot(x, geom = "smoke")
#' qplot(x, geom = "histogram")
#'
#'
#' ggplot(diamonds, aes(carat)) +
#'   geom_smoke(adjust = c(.5, 1, 2), alpha = 1/3)
#'
#' # smoke is fast, but doesn't allow aesthetics
#' # smoke2 does, but is slower
#' if (FALSE) {
#' ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_smoke() +
#'   xlim(55, 70)
#' }
#'
#' # Stacked density plots: if you want to create a stacked density plot, you
#' # probably want to 'count' (density * n) variable instead of the default
#' # density
#'
#' # Loses marginal densities
#' ggplot(diamonds, aes(carat, fill = cut)) +
#'   geom_smoke2(position = "stack")
#' # Preserves marginal densities
#' ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
#'   geom_smoke2(position = "stack")
#'
#' # You can use position="fill" to produce a conditional density estimate
#' ggplot(diamonds, aes(carat, ..count..)) +
#'   geom_smoke2(aes(fill = cut), position = "fill") +
#'   xlim(0, 3)
#'
#'
geom_smoke <- function(mapping = NULL, data = NULL, stat = "smoke",
  position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
  ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSmoke,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(..., na.rm = na.rm)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSmoke <- ggproto("GeomSmoke", GeomArea,
  default_aes = defaults(
    aes(fill = "grey20", weight = 1, colour = NA, alpha = .02),
    GeomArea$default_aes
  )
)







#' @export
#' @rdname geom_smoke
geom_smoke2 <- function(mapping = NULL, data = NULL, stat = "density",
  position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
  ...) {

  s <- seq(0, 1, length.out = 50)
  adjust <- 2^(3*s - 2)

  lapply(adjust, function(a){
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSmoke2,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(..., adjust = a, na.rm = na.rm)
    )
  })

}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSmoke2 <- ggproto("GeomSmoke2", GeomArea,
  default_aes = defaults(
    aes(fill = "grey20", weight = 1, colour = NA, alpha = .02),
    GeomArea$default_aes
  )
)
