#' Dodge overlapping objects side-to-side
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position. `position_dodge()` requires the grouping variable to be
#' be specified in the global or `geom_*` layer. Unlike `position_dodge()`,
#' `position_dodge2()` works without a grouping variable in a layer.
#' `position_dodge2()` works with bars and rectangles, but is
#' particularly useful for arranging box plots, which
#' can have variable widths.
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @param preserve Should dodging preserve the `"total"` width of all elements
#'    at a position, or the width of a `"single"` element?
#' @param orientation Fallback orientation when the layer or the data does not
#'    indicate an explicit orientation, like `geom_point()`. Can be `"x"`
#'    (default) or `"y"`.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
#' @family position adjustments
#' @aesthetics PositionDodge
#'
#' @export
#' @examples
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "dodge2")
#'
#' # By default, dodging with `position_dodge2()` preserves the total width of
#' # the elements. You can choose to preserve the width of each element with:
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = position_dodge2(preserve = "single"))
#'
#' \donttest{
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(position="dodge2")
#' # see ?geom_bar for more examples
#'
#' # In this case a frequency polygon is probably a better choice
#' ggplot(diamonds, aes(price, colour = cut)) +
#'   geom_freqpoly()
#' }
#'
#' # Dodging with various widths -------------------------------------
#' # To dodge items with different widths, you need to be explicit
#' df <- data.frame(
#'   x = c("a","a","b","b"),
#'   y = 2:5,
#'   g = rep(1:2, 2)
#' )
#' p <- ggplot(df, aes(x, y, group = g)) +
#'   geom_col(position = "dodge", fill = "grey50", colour = "black")
#' p
#'
#' # A line range has no width:
#' p + geom_linerange(aes(ymin = y - 1, ymax = y + 1), position = "dodge")
#'
#' # So you must explicitly specify the width
#' p + geom_linerange(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   position = position_dodge(width = 0.9)
#' )
#'
#' # The same principle applies to error bars, which are usually
#' # narrower than the bars
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = "dodge"
#' )
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = position_dodge(width = 0.9)
#' )
#'
#' # Box plots use position_dodge2 by default, and bars can use it too
#' ggplot(mpg, aes(factor(year), displ)) +
#'   geom_boxplot(aes(colour = hwy < 30))
#'
#' ggplot(mpg, aes(factor(year), displ)) +
#'   geom_boxplot(aes(colour = hwy < 30), varwidth = TRUE)
#'
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = position_dodge2(preserve = "single"))
#'
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = position_dodge2(preserve = "total"))
position_dodge <- function(width = NULL, preserve = "total", orientation = "x",
                           reverse = FALSE) {
  check_bool(reverse)
  ggproto(NULL, PositionDodge,
    width = width,
    preserve = arg_match0(preserve, c("total", "single")),
    orientation = arg_match0(orientation, c("x", "y")),
    reverse = reverse
  )
}

#' @rdname Position
#' @format NULL
#' @usage NULL
#' @export
PositionDodge <- ggproto("PositionDodge", Position,
  width = NULL,
  preserve = "total",
  orientation = "x",
  reverse = NULL,
  default_aes = aes(order = NULL),

  setup_params = function(self, data) {

    flipped_aes <- has_flipped_aes(data, default = self$orientation == "y")
    check_required_aesthetics(
      if (flipped_aes) "y|ymin" else "x|xmin",
      names(data), snake_class(self)
    )

    data <- flip_data(data, flipped_aes)
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      cli::cli_warn(c(
        "Width not defined",
        "i" = "Set with {.code position_dodge(width = ...)}"
      ))
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      data$xmin <- data$xmin %||% data$x
      cols <- intersect(colnames(data), c("group", "PANEL", "xmin"))
      n <- vec_unique(data[cols])
      n <- vec_group_id(n[setdiff(cols, "group")])
      n <- max(tabulate(n, attr(n, "n")))
    }

    list(
      width = self$width,
      n = n,
      flipped_aes = flipped_aes,
      reverse = self$reverse %||% FALSE
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)

    if (!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x <- (data$xmin + data$xmax) / 2
    }

    data$order <- xtfrm( # xtfrm makes anything 'sortable'
      data$order %||% ave(data$group, data$x, data$PANEL, FUN = match_sorted)
    )
    if (isTRUE(params$reverse)) {
      data$order <- -data$order
    }
    if (is.null(params$n)) { # preserve = "total"
      data$order <- ave(data$order, data$x, data$PANEL, FUN = match_sorted)
    } else { # preserve = "single"
      data$order <- match_sorted(data$order)
    }
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide(
      data,
      params$width,
      name = "position_dodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE,
      reverse = !params$reverse # for consistency with `position_dodge2()`
    )
    flip_data(collided, params$flipped_aes)
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodge <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- vec_unique_count(df$group)
  }

  if (n == 1)
    return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- df$order %||% match_sorted(df$group)

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - 0.5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}

match_sorted <- function(x, y = x, ...) {
  vec_match(x, vec_sort(unique0(y), ...))
}
