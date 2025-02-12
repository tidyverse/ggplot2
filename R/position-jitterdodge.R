#' Simultaneously dodge and jitter
#'
#' This is primarily used for aligning points generated through
#' `geom_point()` with dodged boxplots (e.g., a `geom_boxplot()` with
#' a fill aesthetic supplied).
#'
#' @family position adjustments
#' @param jitter.width degree of jitter in x direction. Defaults to 40% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param dodge.width the amount to dodge in the x direction. Defaults to 0.75,
#'   the default `position_dodge()` width.
#' @inheritParams position_jitter
#' @inheritParams position_dodge
#' @export
#' @examples
#' set.seed(596)
#' dsub <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
position_jitterdodge <- function(jitter.width = NULL, jitter.height = 0,
                                 dodge.width = 0.75, reverse = FALSE,
                                 seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  check_bool(reverse)

  ggproto(NULL, PositionJitterdodge,
    jitter.width = jitter.width,
    jitter.height = jitter.height,
    dodge.width = dodge.width,
    reverse = reverse,
    seed = seed
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionJitterdodge <- ggproto("PositionJitterdodge", Position,
  jitter.width = NULL,
  jitter.height = NULL,
  dodge.width = NULL,
  reverse = NULL,

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    width <- self$jitter.width %||% (resolution(data$x, zero = FALSE, TRUE) * 0.4)

    ndodge <- vec_unique(data[c("group", "PANEL", "x")])
    ndodge <- vec_group_id(ndodge[c("PANEL", "x")])
    ndodge <- max(tabulate(ndodge, attr(ndodge, "n")))

    list(
      dodge.width = self$dodge.width %||% 0.75,
      jitter.height = self$jitter.height %||% 0,
      jitter.width = width / (ndodge + 2),
      seed = self$seed,
      flipped_aes = flipped_aes,
      reverse = self$reverse %||% FALSE
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    data <- collide(
      data,
      params$dodge.width,
      "position_jitterdodge",
      strategy = pos_dodge,
      check.width = FALSE,
      reverse = !params$reverse # for consistency with `position_dodge2()`
    )
    data <- compute_jitter(data, params$jitter.width, params$jitter.height, params$seed)
    flip_data(data, params$flipped_aes)
  }
)
