#' Points, jittered to reduce overplotting.
#'
#' The jitter geom is a convenient default for geom_point with position =
#' 'jitter'. It's a useful way of handling overplotting caused by discreteness
#' in smaller datasets.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "jitter")}
#'
#' @inheritParams geom_point
#' @inheritParams position_jitter
#' @param inward When \code{TRUE}, use \code{\link{position_jitterin}()}
#'   instead of \code{position_jitter()}.
#' @seealso \code{\link{geom_point}} for regular, unjittered points,
#'   \code{\link{geom_boxplot}} for another way of looking at the
#'   conditional distribution of a variable
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
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitter()
#' ggplot(mpg, aes(cyl, hwy)) + geom_jitter(width = 0.25)
#'
#' # Use larger width/height to completely smooth away discreteness
#' ggplot(mpg, aes(cty, hwy)) + geom_jitter()
#' ggplot(mpg, aes(cty, hwy)) + geom_jitter(width = 0.5, height = 0.5)
#'
#' # With binary data, use inward to bound jitter in the data range
#' movies_sub <- movies[sample(nrow(movies), 1000), ]
#' ggplot(movies_sub, aes(year, Comedy)) +
#'   geom_jitter(inward = TRUE, alpha = 0.5) +
#'   stat_smooth(method = "glm", family = "binomial", level = 0)
geom_jitter <- function(mapping = NULL, data = NULL,
                        width = NULL, height = NULL, inward = FALSE,
                        stat = "identity", position = "jitter",
                        na.rm = FALSE, ...) {
  if (!missing(width) || !missing(height) || !missing(inward)) {
    if (!missing(position)) {
      stop("Specify either `position` or `width`/`height`/`inward`",
        call. = FALSE)
    }

    if (!missing(inward) && inward) {
      position <- position_jitterin(width = width, height = height)
    } else {
      position <- position_jitter(width = width, height = height)
    }
  }

  GeomJitter$new(mapping = mapping, data = data, stat = stat,
    position = position, na.rm = na.rm, ...)
}

GeomJitter <- proto(GeomPoint, {
  objname <- "jitter"

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionJitter
})
