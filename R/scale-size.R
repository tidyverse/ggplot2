#' Scales for area or radius
#'
#' `scale_size()` scales area, `scale_radius()` scales radius. The size
#' aesthetic is most commonly used for points and text, and humans perceive
#' the area of points (not their radius), so this provides for optimal
#' perception. `scale_size_area()` ensures that a value of 0 is mapped
#' to a size of 0. `scale_size_binned()` is a binned version of `scale_size()` that
#' scales by area (but does not ensure 0 equals an area of zero). For a binned
#' equivalent of `scale_size_area()` use `scale_size_binned_area()`.
#'
#' @note Historically the size aesthetic was used for two different things:
#'   Scaling the size of object (like points and glyphs) and scaling the width
#'   of lines. From ggplot2 3.4.0 the latter has been moved to its own linewidth
#'   aesthetic. For backwards compatibility using size is still possible, but it
#'   is highly advised to switch to the new linewidth aesthetic for these cases.
#'
#' @name scale_size
#' @inheritParams continuous_scale
#' @inheritParams binned_scale
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @seealso
#' [scale_size_area()] if you want 0 values to be mapped to points with size 0.
#' [scale_linewidth()] if you want to scale the width of lines.
#'
#' The documentation for [differentiation related aesthetics][aes_linetype_size_shape].
#'
#' The `r link_book("size section", "scales-other#sec-scale-size")`
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
                                  limits = NULL, range = NULL,
                                  transform = "identity",
                                  trans = deprecated(),
                                  guide = "legend",
                                  aesthetics = "size") {
  palette <- if (!is.null(range)) pal_area(range) else NULL
  continuous_scale(aesthetics, palette = palette, name = name,
    breaks = breaks, labels = labels, limits = limits,
    transform = transform, trans = trans, guide = guide)
}

#' @rdname scale_size
#' @export
scale_size <- scale_size_continuous

#' @rdname scale_size
#' @export
scale_radius <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         transform = "identity", trans = deprecated(),
                         guide = "legend", aesthetics = "size") {
  continuous_scale(aesthetics, palette = pal_rescale(range), name = name,
    breaks = breaks, labels = labels, limits = limits, transform = transform,
    trans = trans, guide = guide)
}

#' @rdname scale_size
#' @export
scale_size_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                              limits = NULL, range = NULL, n.breaks = NULL,
                              nice.breaks = TRUE, transform = "identity",
                              trans = deprecated(), guide = "bins",
                              aesthetics = "size") {
  palette <- if (!is.null(range)) pal_area(range) else NULL
  binned_scale(aesthetics, palette = palette, name = name,
               breaks = breaks, labels = labels, limits = limits,
               transform = transform, trans = trans, n.breaks = n.breaks,
               nice.breaks = nice.breaks, guide = guide)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_discrete <- function(...) {
  cli::cli_warn("Using {.field size} for a discrete variable is not advised.")
  args <- list2(...)
  args$call <- args$call %||% current_call()
  exec(scale_size_ordinal, !!!args)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_ordinal <- function(name = waiver(), ..., range = NULL, aesthetics = "size") {
  palette <- if (!is.null(range)) {
    function(n) sqrt(seq(range[1]^2, range[2]^2, length.out = n))
  } else {
    NULL
  }
  discrete_scale(aesthetics, name = name, palette = palette, ...)
}

#' @inheritDotParams continuous_scale -aesthetics -scale_name -palette -rescaler -expand -position
#' @param max_size Size of largest points.
#' @export
#' @rdname scale_size
scale_size_area <- function(name = waiver(), ..., max_size = 6, aesthetics = "size") {
  continuous_scale(
    aesthetics, name = name,
    palette = abs_area(max_size),
    rescaler = rescale_max, ...
  )
}

#' @export
#' @rdname scale_size
scale_size_binned_area <- function(name = waiver(), ..., max_size = 6, aesthetics = "size") {
  binned_scale(
    aesthetics, name = name,
    palette = abs_area(max_size),
    rescaler = rescale_max, ...
  )
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_datetime <- function(name = waiver(), ..., range = NULL, aesthetics = "size") {
  palette <- if (!is.null(range)) pal_area(range) else NULL
  datetime_scale(aesthetics, "time", name = name, palette = palette, ...)
}

#' @rdname scale_size
#' @export
#' @usage NULL
scale_size_date <- function(name = waiver(), ..., range = NULL, aesthetics = "size") {
  palette <- if (!is.null(range)) pal_area(range) else NULL
  datetime_scale(aesthetics, "date", name = name, palette = palette, ...)
}
