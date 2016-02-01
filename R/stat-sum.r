#' @inheritParams layer
#' @inheritParams geom_point
#' @section Computed variables:
#' \describe{
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' }
#' @export
#' @rdname geom_count
stat_sum <- function(mapping = NULL, data = NULL,
                     geom = "point", position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSum,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSum <- ggproto("StatSum", Stat,
  default_aes = aes(size = ..n..),

  required_aes = c("x", "y"),

  compute_panel = function(data, scales) {
    if (is.null(data$weight)) data$weight <- 1

    group_by <- setdiff(intersect(names(data), .all_aesthetics), "weight")

    counts <- plyr::count(data, group_by, wt_var = "weight")
    counts <- plyr::rename(counts, c(freq = "n"), warn_missing = FALSE)
    counts$prop <- stats::ave(counts$n, counts$group, FUN = prop.table)
    counts
  }
)
