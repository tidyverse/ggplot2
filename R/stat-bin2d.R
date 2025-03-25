#' @inheritParams stat_bin
#' @param drop if `TRUE` removes all cells with 0 counts.
#' @export
#' @rdname geom_bin_2d
#' @eval rd_computed_vars(
#'   count    = "number of points in bin.",
#'   density  = "density of points in bin, scaled to integrate to 1.",
#'   ncount   = "count, scaled to maximum of 1.",
#'   ndensity = "density, scaled to a maximum of 1."
#' )
#' @section Controlling binning parameters for the x and y directions:
#' The arguments `bins`, `binwidth`, `breaks`, `center`, and `boundary` can
#' be set separately for the x and y directions. When given as a scalar, one
#' value applies to both directions. When given as a vector of length two,
#' the first is applied to the x direction and the second to the y direction.
#' Alternatively, these can be a named list containing `x` and `y` elements,
#' for example `list(x = 10, y = 20)`.
stat_bin_2d <- function(mapping = NULL, data = NULL,
                        geom = "tile", position = "identity",
                        ...,
                        bins = 30,
                        binwidth = NULL,
                        center = NULL,
                        boundary = NULL,
                        breaks = NULL,
                        drop = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBin2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      bins = bins,
      binwidth = binwidth,
      center = center,
      boundary = boundary,
      breaks = breaks,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
#' @rdname geom_bin_2d
#' @usage NULL
stat_bin2d <- stat_bin_2d

#' @rdname ggplot2-ggproto
#' @include stat-summary-2d.R
#' @format NULL
#' @usage NULL
#' @export
StatBin2d <- ggproto(
  "StatBin2d", StatSummary2d,
  default_aes = aes(weight = 1, fill = after_stat(count)),
  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           breaks = NULL, origin = NULL, drop = TRUE,
                           boundary = NULL, closed = NULL, center = NULL) {

    data$z <- data$weight %||% 1
    data$weight <- NULL

    # For backward compatibility, boundary defaults to 0
    boundary <- boundary %||% if (is.null(center)) list(x = 0, y = 0)

    out <- StatSummary2d$compute_group(
      data, scales, binwidth = binwidth, bins = bins, breaks = breaks,
      drop = drop, fun = "sum", boundary = boundary, closed = closed,
      center = center
    )

    out$count <- out$value
    out$ncount <- out$count / max(out$count, na.rm = TRUE)
    out$density <- out$count / sum(out$count, na.rm = TRUE)
    out$ndensity <- out$density / max(out$density, na.rm = TRUE)
    out
  }
)

dual_param <- function(x, default = list(x = NULL, y = NULL)) {
  if (is.null(x)) {
    default
  } else if (length(x) == 2) {
    if (is.list(x) && !is.null(names(x))) {
      x
    } else {
      list(x = x[[1]], y = x[[2]])
    }
  } else {
    list(x = x, y = x)
  }
}
