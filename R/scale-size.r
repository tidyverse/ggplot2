#' Scales for area or radius
#'
#' `scale_size` scales area, `scale_radius` scales radius. The size
#' aesthetic is most commonly used for points and text, and humans perceive
#' the area of points (not their radius), so this provides for optimal
#' perception. `scale_size_area` ensures that a value of 0 is mapped
#' to a size of 0.
#'
#' @name scale_size
#' @inheritParams continuous_scale
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with.
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
                                  trans = "identity", guide = "legend", aesthetics = "size") {
  continuous_scale(aesthetics, "area", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         trans = "identity", guide = "legend", aesthetics = "size") {
  continuous_scale(aesthetics, "radius", rescale_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_size <- scale_size_continuous

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_discrete <- function(...) {
  warning("Using size for a discrete variable is not advised.", call. = FALSE)
  scale_size_ordinal(...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_ordinal <- function(..., range = c(2, 6), aesthetics = "size") {
  discrete_scale(
    aesthetics,
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
scale_size_area <- function(..., max_size = 6, aesthetics = "size") {
  continuous_scale(aesthetics, "area",
    palette = abs_area(max_size),
    rescaler = rescale_max, ...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_datetime <- function(..., range = c(1, 6), aesthetics = "size") {
  datetime_scale(aesthetics, "time", palette = area_pal(range), ...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_date <- function(..., range = c(1, 6), aesthetics = "size") {
  datetime_scale(aesthetics, "date", palette = area_pal(range), ...)
}
