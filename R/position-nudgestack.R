#' Simultaneously nudge and stack
#'
#' This is primarily used for set stacked columns between the ticks on the
#' x-axis.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @param vjust Vertical adjustment for geoms that have a position
#'   (like points or lines), not a dimension (like bars or areas). Set to
#'   `0` to align with the bottom, `0.5` for the middle,
#'   and `1` (the default) for the top.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
#' @export
#' @examples
#' ESM <- tsbox::ts_tbl(EuStockMarkets)
#'
#' ESM_prep <- ESM %>%
#'   dplyr::mutate(time = as.Date(paste0(format(time, "%Y-%m"), "-1"))) %>%
#'   dplyr::group_by(id, time) %>%
#'   dplyr::summarize(value = mean(value)) %>%
#'   dplyr::filter(time >= "1995-01-01" & time < "1998-01-01")
#'
#' ggplot(data = ESM_prep, mapping = aes(x = time, y = value, fill = id)) +
#'   geom_col(position = position_nudgestack(x = 15))
position_nudgestack <- function(x = 0, y = 0, vjust = 1, reverse = FALSE) {
  ggproto(NULL, PositionNudgeStack,
    x = x,
    y = y,
    vjust = vjust,
    reverse = reverse
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeStack <- ggproto("PositionNudgeStack", Position,
  x = 0,
  y = 0,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,

  setup_params = function(self, data) {
    list(
      x = self$x,
      y = self$y,
      var = if (!is.null(self$var)) self$var else stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse
    )
  },

  setup_data = function(self, data, params) {
    if (is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
      y = data$y,
      ymax = ifelse(data$ymax == 0, data$ymin, data$ymax)
    )

    remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_stack"
    )
  },

  compute_layer = function(self, data, params, layout) {
    if (is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]

    if (any(negative)) {
      neg <- collide(neg, NULL, "position_stack", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }
    if (any(!negative)) {
      pos <- collide(pos, NULL, "position_stack", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }

    data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))), ]

    # transform only the dimensions for which non-zero nudging is requested
    if (any(params$x != 0)) {
      if (any(params$y != 0)) {
        transform_position(data, function(x) x + params$x, function(y) y + params$y)
      } else {
        transform_position(data, function(x) x + params$x, NULL)
      }
    } else if (any(params$y != 0)) {
      transform_position(data, NULL, function(y) y + params$y)
    } else {
      data # if both x and y are 0 we don't need to transform
    }
  }
)
