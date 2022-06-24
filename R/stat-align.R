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
    params$unique_loc <- unique(sort(data[[flipped_names(params$flipped_aes)$x]]))
    params
  },

  compute_group = function(data, scales, flipped_aes = NA, unique_loc = NULL) {
    data <- flip_data(data, flipped_aes)
    if (length(unique(data$x)) == 1) {
      # Not enough data to align
      return(new_data_frame())
    }
    y_val <- approxfun(data$x, data$y)(unique_loc)
    keep <- !is.na(y_val)
    x_val <- unique_loc[keep]
    y_val <- y_val[keep]
    data_aligned <- cbind(
      x = x_val,
      y = y_val,
      data[1, setdiff(names(data), c("x", "y"))],
      flipped_aes = flipped_aes
    )
    flip_data(data_aligned, flipped_aes)
  }
)
