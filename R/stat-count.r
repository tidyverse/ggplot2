#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#' @seealso [stat_bin()], which bins data in ranges and counts the
#'   cases in each range. It differs from `stat_count()`, which counts the
#'   number of cases at each `x` position (without binning into ranges).
#'   [stat_bin()] requires continuous `x` data, whereas
#'   `stat_count()` can be used for both discrete and continuous `x` data.
#'
#' @export
#' @rdname geom_bar
stat_count <- function(mapping = NULL, data = NULL,
                       geom = "bar", position = "stack",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       orientation = NA,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  params <- list2(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    ...
  )

  layer(
    data = data,
    mapping = mapping,
    stat = StatCount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatCount <- ggproto("StatCount", Stat,
  required_aes = "x|y",

  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),

  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }

    if (is.null(params$width)) {
      x <- if (params$flipped_aes) "y" else "x"
      params$width <- resolution(data[[x]]) * 0.9
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(self, data, scales, width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    bars <- new_data_frame(list(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique(x)),
      width = width,
      flipped_aes = flipped_aes
    ), n = length(count))
    flip_data(bars, flipped_aes)
  }
)
