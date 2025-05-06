#' Jitter points to avoid overplotting
#'
#' Counterintuitively adding random noise to a plot can sometimes make it
#' easier to read. Jittering is particularly useful for small datasets with
#' at least one discrete position.
#'
#' @family position adjustments
#' @param width,height Amount of vertical and horizontal jitter. The jitter
#'   is added in both positive and negative directions, so the total spread
#'   is twice the value specified here.
#'
#'   If omitted, defaults to 40% of the resolution of the data: this means the
#'   jitter values will occupy 80% of the implied bins. Categorical data
#'   is aligned on the integers, so a width or height of 0.5 will spread the
#'   data so it's not possible to see the distinction between the categories.
#' @param seed A random seed to make the jitter reproducible.
#'   Useful if you need to apply the same jitter twice, e.g., for a point and
#'   a corresponding label.
#'   The random seed is reset after jittering.
#'   If `NA` (the default value), the seed is initialised with a random value;
#'   this makes sure that two subsequent calls start with a different seed.
#'   Use `NULL` to use the current random seed and also avoid resetting
#'   (the behaviour of \pkg{ggplot} 2.2.1 and earlier).
#' @export
#' @examples
#' # Jittering is useful when you have a discrete position, and a relatively
#' # small number of points
#' # take up as much space as a boxplot or a bar
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(colour = "grey50") +
#'   geom_jitter()
#'
#' # If the default jittering is too much, as in this plot:
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitter()
#'
#' # You can adjust it in two ways
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitter(width = 0.1, height = 0.1)
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))
#'
#' # Create a jitter object for reproducible jitter:
#' jitter <- position_jitter(width = 0.1, height = 0.1, seed = 0)
#' ggplot(mtcars, aes(am, vs)) +
#'   geom_point(position = jitter) +
#'   geom_point(position = jitter, color = "red", aes(am + 0.2, vs + 0.2))
position_jitter <- function(width = NULL, height = NULL, seed = NA) {
  ggproto(NULL, PositionJitter,
    width = width,
    height = height,
    seed = seed
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionJitter <- ggproto("PositionJitter", Position,
  seed = NA,
  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    if (!is.null(self$seed) && is.na(self$seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    } else {
      seed <- self$seed
    }
    list(
      width = self$width,
      height = self$height,
      seed = seed
    )
  },

  compute_panel = function(self, data, params, scales) {
    compute_jitter(data, params$width, params$height, seed = params$seed)
  }
)

compute_jitter <- function(data, width = NULL, height = NULL, seed = NA) {

  width  <- width  %||% (resolution(data$x, zero = FALSE, TRUE) * 0.4)
  height <- height %||% (resolution(data$y, zero = FALSE, TRUE) * 0.4)

  trans_x <- if (width > 0)  function(x) jitter(x, amount = width)
  trans_y <- if (height > 0) function(x) jitter(x, amount = height)

  x_aes <- intersect(ggplot_global$x_aes, names(data))
  x <- if (length(x_aes) == 0) 0 else data[[x_aes[1]]]

  y_aes <- intersect(ggplot_global$y_aes, names(data))
  y <- if (length(y_aes) == 0) 0 else data[[y_aes[1]]]

  jitter <- data_frame0(x = x, y = y, .size = nrow(data))
  jitter <- with_seed_null(seed, transform_position(jitter, trans_x, trans_y))

  x_jit <- jitter$x - x
  x_jit[is.infinite(x)] <- 0

  y_jit <- jitter$y - y
  y_jit[is.infinite(y)] <- 0

  transform_position(data, function(x) x + x_jit, function(x) x + y_jit)
}
