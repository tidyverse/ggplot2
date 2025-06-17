#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
StatKsmooth <- ggproto("StatKsmooth", Stat,
                       required_aes = c("x", "y"),
                       compute_group = function(data, scales, kernel = "box", bandwidth = 0.5) {
                         as.data.frame(ksmooth(x = data$x, y = data$y, kernel = kernel, bandwidth = bandwidth))
                       }
)

#' Apply kernel regression smoother
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams stats::ksmooth
#' @examples
#' ggplot(economics, aes(date, psavert)) +
#'   geom_point(alpha = 0.25) +
#'   stat_ksmooth(bandwidth = 180, kernel = "normal")
#'
stat_ksmooth <- function(mapping = NULL, data = NULL, geom = "line",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, kernel = "box", bandwidth = 0.5,
                         ...) {
  layer(
    stat = StatKsmooth, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(kernel = kernel, bandwidth = bandwidth, ...)
  )
}
