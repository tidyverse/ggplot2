#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' @description
#' The `brewer` scales provides sequential, diverging and qualitative
#' colour schemes from ColorBrewer. These are particularly well suited to
#' display discrete values on a map. See \url{http://colorbrewer2.org} for
#' more information.
#'
#' @note
#' The `distiller` scales extend brewer to continuous scales by smoothly
#' interpolating 6 colours from any palette to a continuous scale.
#'
#' @details
#' The `brewer` scales were carefully designed and tested on discrete data.
#' They were not designed to be extended to continuous data, but results often
#' look good. Your mileage may vary.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'   \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'   \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#'
#' @inheritParams scales::brewer_pal
#' @inheritParams scale_colour_hue
#' @inheritParams scale_colour_gradient
#' @inheritParams scales::gradient_n_pal
#' @param ... Other arguments passed on to [discrete_scale()] or, for
#'   `distiller` scales, [continuous_scale()] to control name,
#'   limits, breaks, labels and so forth.
#' @family colour scales
#' @rdname scale_brewer
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity)))
#' d + scale_colour_brewer()
#'
#' # Change scale label
#' d + scale_colour_brewer("Diamond\nclarity")
#'
#' # Select brewer palette to use, see ?scales::brewer_pal for more details
#' d + scale_colour_brewer(palette = "Greens")
#' d + scale_colour_brewer(palette = "Set1")
#'
#' \donttest{
#' # scale_fill_brewer works just the same as
#' # scale_colour_brewer but for fill colours
#' p <- ggplot(diamonds, aes(x = price, fill = cut)) +
#'   geom_histogram(position = "dodge", binwidth = 1000)
#' p + scale_fill_brewer()
#' # the order of colour can be reversed
#' p + scale_fill_brewer(direction = -1)
#' # the brewer scales look better on a darker background
#' p + scale_fill_brewer(direction = -1) + theme_dark()
#' }
#'
#' # Use distiller variant with continous data
#' v <- ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density))
#' v
#' v + scale_fill_distiller()
#' v + scale_fill_distiller(palette = "Spectral")
scale_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "colour") {
  discrete_scale(aesthetics, "brewer", brewer_pal(type, palette, direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "fill") {
  discrete_scale(aesthetics, "brewer", brewer_pal(type, palette, direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  continuous_scale(aesthetics, "distiller",
    gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, guide = guide, ...)
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}

#' @export
#' @rdname scale_brewer
scale_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  continuous_scale(aesthetics, "distiller",
    gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, guide = guide, ...)
}

# icon.brewer <- function() {
#   rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width = 0.21,
#     gp = gpar(fill = RColorBrewer::brewer.pal(5, "PuOr"), col = NA)
#   )
# }
