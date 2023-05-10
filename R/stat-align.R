#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @rdname geom_ribbon
stat_align <- function(mapping = NULL, data = NULL,
                       geom = "area", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatAlign,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatAlign <- ggproto("StatAlign", Stat,
  extra_params = c("na.rm", "orientation"),
  required_aes = c("x", "y"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  compute_panel = function(self, data, scales, flipped_aes, ...) {
    if (empty(data)) {
      return(data_frame0())
    }

    names <- flipped_names(flipped_aes)
    x <- data[[names$x]]
    y <- data[[names$y]]

    if (is_unique(data$group)) {
      # No need for interpolation
      cross <- x[0]
    } else {
      # Find positions where 0 is crossed
      pivot <- vec_unrep(data_frame0(group = data$group, y = y < 0))
      group_ends <- cumsum(vec_unrep(pivot$key$group)$times)
      pivot <- cumsum(pivot$times)[-group_ends]
      cross <- -y[pivot] * (x[pivot + 1] - x[pivot]) /
        (y[pivot + 1] - y[pivot]) + x[pivot]
    }

    unique_loc <- unique(sort(c(x, cross)))
    adjust     <- diff(range(unique_loc, na.rm = TRUE)) * 0.001
    adjust     <- min(adjust, min(diff(unique_loc)) / 3)
    unique_loc <- unique(sort(c(
      unique_loc - adjust, unique_loc, unique_loc + adjust
    )))

    ggproto_parent(Stat, self)$compute_panel(
      data, scales, flipped_aes = flipped_aes, unique_loc = unique_loc,
      adjust = adjust, ...
    )
  },

  compute_group = function(data, scales, flipped_aes = NA, unique_loc = NULL, adjust = 0) {
    data <- flip_data(data, flipped_aes)
    if (is_unique(data$x)) {
      # Not enough data to align
      return(new_data_frame())
    }
    # Sort out multiple observations at the same x
    if (anyDuplicated(data$x)) {
      data <- dapply(data, "x", function(d) {
        if (nrow(d) == 1) return(d)
        d <- d[c(1, nrow(d)), ]
        d$x[1] <- d$x[1] - adjust
        d
      })
    }
    y_val <- approxfun(data$x, data$y)(unique_loc)
    keep <- !is.na(y_val)
    x_val <- unique_loc[keep]
    y_val <- y_val[keep]
    x_val <- c(min(x_val) - adjust, x_val, max(x_val) + adjust)
    y_val <- c(0, y_val, 0)

    data_aligned <- data_frame0(
      x = x_val,
      y = y_val,
      data[1, setdiff(names(data), c("x", "y"))],
      align_padding = c(TRUE, rep(FALSE, length(x_val) - 2), TRUE),
      flipped_aes = flipped_aes
    )
    flip_data(data_aligned, flipped_aes)
  }
)
