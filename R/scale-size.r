#' Scales for area or radius
#'
#' `scale_size` scales area, `scale_radius` scales radius. The size
#' aesthetic is most commonly used for points and text, and humans perceive
#' the area of points (not their radius), so this provides for optimal
#' perception. `scale_size_area` ensures that a value of 0 is mapped
#' to a size of 0. `scale_size_binned` is a binned version of `scale_size` that
#' scales by area (but does not ensure 0 equals an area of zero). For a binned
#' equivalent of `scale_size_area` use `scale_size_binned_area`.
#'
#' @name scale_size
#' @inheritParams continuous_scale
#' @inheritParams binned_scale
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @seealso [scale_size_area()] if you want 0 values to be mapped
#'   to points with size 0.
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy, size = hwy)) +
#'    geom_point()
#' p
#' p + scale_size("Highway mpg")
#' p + scale_size(range = c(0, 10))
#'
#' # If you want zero value to have zero size, use scale_size_area:
#' p + scale_size_area()
#'
#' # Binning can sometimes make it easier to match the scaled data to the legend
#' p + scale_size_binned()
#'
#' # This is most useful when size is a count
#' ggplot(mpg, aes(class, cyl)) +
#'   geom_count() +
#'   scale_size_area()
#'
#' # If you want to map size to radius (usually bad idea), use scale_radius
#' p + scale_radius()
NULL

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend") {
  continuous_scale("size", "area", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_size <- scale_size_continuous

#' @rdname scale_size
#' @export
scale_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         trans = "identity", guide = "legend") {
  continuous_scale("size", "radius", rescale_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_size_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                              limits = NULL, range = c(1, 6), n.breaks = NULL,
                              nice.breaks = TRUE, trans = "identity", guide = "bins") {
  binned_scale("size", "area_b", area_pal(range), name = name,
               breaks = breaks, labels = labels, limits = limits, trans = trans,
               n.breaks = n.breaks, nice.breaks = nice.breaks, guide = guide)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_discrete <- function(...) {
  warn("Using size for a discrete variable is not advised.")
  scale_size_ordinal(...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_ordinal <- function(..., range = c(2, 6)) {
  force(range)

  discrete_scale(
    "size",
    "size_d",
    function(n) {
      area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
      sqrt(area)
    },
    ...
  )
}

#' @inheritDotParams continuous_scale -aesthetics -scale_name -palette -rescaler
#' @param max_size Size of largest points.
#' @export
#' @rdname scale_size
scale_size_area <- function(..., max_size = 6) {
  continuous_scale("size", "area",
    palette = abs_area(max_size),
    rescaler = rescale_max, ...)
}

#' @export
#' @rdname scale_size
scale_size_binned_area <- function(..., max_size = 6) {
  binned_scale("size", "area_b",
               palette = abs_area(max_size),
               rescaler = rescale_max, ...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_datetime <- function(..., range = c(1, 6)) {
  datetime_scale("size", "time", palette = area_pal(range), ...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_date <- function(..., range = c(1, 6)) {
  datetime_scale("size", "date", palette = area_pal(range), ...)
}
