#' \code{stat_count} counts the number of cases at each x position. If you want
#' to bin the data in ranges, you should use \code{\link{stat_bin}} instead.
#'
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#' @seealso \code{\link{stat_bin}}, which bins data in ranges and counts the
#'   cases in each range. It differs from \code{stat_count}, which counts the
#'   number of cases at each x position (without binning into ranges).
#'   \code{\link{stat_bin}} requires continuous x data, whereas
#'   \code{stat_count} can be used for both discrete and continuous x data.
#'
#' @export
#' @rdname geom_bar
stat_count <- function(mapping = NULL, data = NULL,
                       geom = "bar", position = "stack",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatCount <- ggproto("StatCount", Stat,
  required_aes = "x",
  default_aes = aes(y = ..count..),

  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },

  compute_group = function(self, data, scales, width = NULL) {
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))
    width <- width %||% (resolution(x) * 0.9)

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    data.frame(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique(x)),
      width = width
    )
  }
)
