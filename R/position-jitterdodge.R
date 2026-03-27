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
#'
#' @section Dodge grouping:
#' When a `fill` aesthetic is present and discrete, `position_jitterdodge()`
#' uses it to determine dodge grouping. This ensures points align with dodged
#' boxplots even when additional discrete aesthetics like `colour` are mapped,
#' which would otherwise inflate the implicit group (see [aes_group_order]).
#'
#' If no discrete `fill` is present, dodging falls back to the standard
#' `group` aesthetic. You can always override grouping explicitly with
#' `aes(group = ...)`.
#'
#' @export
#' @examples
#' set.seed(596)
#' dsub <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
#'
#' # Additional discrete aesthetics like colour no longer misalign
#' # points with boxes — dodge grouping is inferred from fill:
#' \donttest{
#' set.seed(596)
#' df <- data.frame(
#'   x = rep(c("A", "B"), each = 20),
#'   y = rnorm(40),
#'   fill_var = rep(c("g1", "g2"), 20),
#'   colour_var = sample(c(TRUE, FALSE), 40, replace = TRUE)
#' )
#'
#' ggplot(df, aes(x, y, fill = fill_var)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(
#'     aes(colour = colour_var),
#'     position = position_jitterdodge()
#'   )
#' }
position_jitterdodge <- function(jitter.width = NULL, jitter.height = 0,
                                 dodge.width = 0.75, reverse = FALSE,
                                 preserve = "total",
                                 seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  check_bool(reverse)

  ggproto(NULL, PositionJitterdodge,
    jitter.width = jitter.width,
    jitter.height = jitter.height,
    dodge.width = dodge.width,
    preserve = arg_match0(preserve, c("total", "single")),
    reverse = reverse,
    seed = seed
  )
}

# Infer dodge grouping from fill when available, so points align with
# dodged boxplots even when additional discrete aesthetics are present.
jitterdodge_dodge_group <- function(data) {
  if ("fill" %in% names(data) && is_discrete(data[["fill"]])) {
    return(id(data["fill"], drop = TRUE))
  }
  data$group
}

#' @rdname Position
#' @format NULL
#' @usage NULL
#' @export
PositionJitterdodge <- ggproto("PositionJitterdodge", Position,
  jitter.width = NULL,
  jitter.height = NULL,
  dodge.width = NULL,
  reverse = NULL,
  default_aes = aes(order = NULL),
  preserve = "total",

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    width <- self$jitter.width %||% (resolution(data$x, zero = FALSE, TRUE) * 0.4)

    dodge_group <- jitterdodge_dodge_group(data)

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      dodge_data <- data
      dodge_data$group <- dodge_group
      n <- vec_unique(dodge_data[c("group", "PANEL", "x")])
      n <- vec_group_id(n[c("PANEL", "x")])
      n <- max(tabulate(n, attr(n, "n")))
    }

    list(
      dodge.width = self$dodge.width %||% 0.75,
      jitter.height = self$jitter.height %||% 0,
      jitter.width = width / ((n %||% 1) + 2),
      n = n,
      seed = self$seed,
      flipped_aes = flipped_aes,
      reverse = self$reverse %||% FALSE
    )
  },

  setup_data = function(self, data, params) {
    original_group <- data$group
    data$group <- jitterdodge_dodge_group(data)
    data <- PositionDodge$setup_data(data = data, params = params)
    data$group <- original_group
    data
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    original_group <- data$group
    data$group <- jitterdodge_dodge_group(data)
    data <- collide(
      data,
      params$dodge.width,
      name = "position_jitterdodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE,
      reverse = !params$reverse # for consistency with `position_dodge2()`
    )
    data$group <- original_group
    data <- flip_data(data, params$flipped_aes)
    compute_jitter(data, params$jitter.width, params$jitter.height, params$seed)
  }
)
