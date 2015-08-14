#' Horizontal error bars
#'
#' This function is deprecated. Please use the new \code{orient}
#' argument of \code{geom_errorbar()} instead.
#'
#' @seealso \code{\link{geom_errorbar}}: vertical error bars
#' @inheritParams geom_point
#' @export
geom_errorbarh <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, ...) {
  gg_dep("1.0.1", "geom_errorbarh() is deprecated. Please use the new
    `orient` argument of geom_errorbar() instead.")
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomErrorbar,
    position = position,
    flip = TRUE,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...),
    stat_params = list(orient = "h"),
    geom_params = list(orient = "h")
  )
}
