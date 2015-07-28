#' @include geom-path-.r
NULL

#' @export
#' @rdname geom_histogram
geom_freqpoly <- function(mapping = NULL, data = NULL, stat = "bin",
  position = "identity", show.legend = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
