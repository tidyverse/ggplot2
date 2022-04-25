#' Jittered points
#'
#' The jitter geom is a convenient shortcut for
#' `geom_point(position = "jitter")`. It adds a small amount of random
#' variation to the location of each point, and is a useful way of handling
#' overplotting caused by discreteness in smaller datasets.
#'
#' @eval rd_aesthetics("geom", "point")
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams position_jitter
#' @seealso
#'  [geom_point()] for regular, unjittered points,
#'  [geom_boxplot()] for another way of looking at the conditional
#'     distribution of a variable
#' @export
#' @examples
#' p <- ggplot(mpg, aes(cyl, hwy))
#' p + geom_point()
#' p + geom_jitter()
#'
#' # Add aesthetic mappings
#' p + geom_jitter(aes(colour = class))
#'
#' # Use smaller width/height to emphasise categories
#' ggplot(mpg, aes(cyl, hwy)) +
#'   geom_jitter()
#' ggplot(mpg, aes(cyl, hwy)) +
#'   geom_jitter(width = 0.25)
#'
#' # Use larger width/height to completely smooth away discreteness
#' ggplot(mpg, aes(cty, hwy)) +
#'   geom_jitter()
#' ggplot(mpg, aes(cty, hwy)) +
#'   geom_jitter(width = 0.5, height = 0.5)
geom_jitter <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "jitter",
                        ...,
                        width = NULL,
                        height = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (!missing(width) || !missing(height)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg width}/{.arg height} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- position_jitter(width = width, height = height)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}
