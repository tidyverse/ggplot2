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
#' @section Interaction with grouping:
#' When no explicit `group` aesthetic is set, ggplot2 computes groups from the
#' interaction of all discrete aesthetics in the layer (see [aes_group_order]).
#' If your point layer maps additional discrete aesthetics beyond the `fill`
#' used for dodging (e.g., `colour`, `shape`, or `linetype`), the points will
#' be split into more groups than the dodged boxplots, causing misalignment.
#'
#' To fix this, explicitly set `group` to the same variable used for dodging
#' (typically the `fill` variable):
#'
#' \preformatted{geom_point(aes(colour = status, group = fill_var),
#'            position = position_jitterdodge())}
#'
#' @export
#' @examples
#' set.seed(596)
#' dsub <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
#'
#' # When mapping additional discrete aesthetics (e.g. colour), points
#' # can misalign with boxes because the implicit groups are inflated.
#' # Fix by setting group to the fill variable:
#' \donttest{
#' set.seed(596)
#' df <- data.frame(
#'   x = rep(c("A", "B"), each = 20),
#'   y = rnorm(40),
#'   fill_var = rep(c("g1", "g2"), 20),
#'   colour_var = sample(c(TRUE, FALSE), 40, replace = TRUE)
#' )
#'
#' # Misaligned: colour creates extra implicit groups
#' ggplot(df, aes(x, y, fill = fill_var)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(
#'     aes(colour = colour_var),
#'     position = position_jitterdodge()
#'   )
#'
#' # Fixed: explicit group aligns points with boxes
#' ggplot(df, aes(x, y, fill = fill_var)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(
#'     aes(colour = colour_var, group = fill_var),
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

    # Warn when additional discrete aesthetics inflate groups beyond fill
    if ("fill" %in% names(data) && is_discrete(data[["fill"]])) {
      groups_per_pos <- vec_unique(data[c("group", "PANEL", "x")])
      n_groups <- max(tabulate(vec_group_id(groups_per_pos[c("PANEL", "x")])))
      fills_per_pos <- vec_unique(data[c("fill", "PANEL", "x")])
      n_fills <- max(tabulate(vec_group_id(fills_per_pos[c("PANEL", "x")])))
      if (n_groups > n_fills) {
        cli::cli_warn(c(
          "Dodge groups are larger than the number of {.field fill} values.",
          "i" = paste(
            "This can happen when additional discrete aesthetics (e.g.,",
            "{.field colour}) inflate the implicit grouping."
          ),
          "i" = paste(
            "Set {.code aes(group = <fill variable>)} to align points",
            "with the dodged layer."
          )
        ))
      }
    }

    width <- self$jitter.width %||% (resolution(data$x, zero = FALSE, TRUE) * 0.4)

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      n <- vec_unique(data[c("group", "PANEL", "x")])
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
    PositionDodge$setup_data(data = data, params = params)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    data <- collide(
      data,
      params$dodge.width,
      name = "position_jitterdodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE,
      reverse = !params$reverse # for consistency with `position_dodge2()`
    )
    data <- flip_data(data, params$flipped_aes)
    compute_jitter(data, params$jitter.width, params$jitter.height, params$seed)
  }
)
