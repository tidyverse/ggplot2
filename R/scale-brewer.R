#' Sequential, diverging and qualitative colour scales from ColorBrewer
#'
#' @description
#' The `brewer` scales provide sequential, diverging and qualitative
#' colour schemes from ColorBrewer. These are particularly well suited to
#' display discrete values on a map. See \url{https://colorbrewer2.org} for
#' more information.
#'
#' @note
#' The `distiller` scales extend `brewer` scales by smoothly
#' interpolating 7 colours from any palette to a continuous scale.
#' The `distiller` scales have a default direction = -1. To reverse, use direction = 1.
#' The `fermenter` scales provide binned versions of the `brewer` scales.
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
#' Modify the palette through the `palette` argument.
#'
#' @inheritParams scales::pal_brewer
#' @inheritParams scale_colour_hue
#' @inheritParams scale_colour_gradient
#' @inheritParams scales::pal_gradient_n
#' @param palette If a string, will use that named palette. If a number, will index into
#'   the list of palettes of appropriate `type`. The list of available palettes can found
#'   in the Palettes section.
#' @param ... Other arguments passed on to [discrete_scale()], [continuous_scale()],
#'   or [binned_scale()], for `brewer`, `distiller`, and `fermenter` variants
#'   respectively, to control name, limits, breaks, labels and so forth.
#' @family colour scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#' @rdname scale_brewer
#' @export
#' @examples
#' set.seed(596)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity)))
#' d + scale_colour_brewer()
#'
#' # Change scale label
#' d + scale_colour_brewer("Diamond\nclarity")
#'
#' # Select brewer palette to use, see ?scales::pal_brewer for more details
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
#' p +
#'   scale_fill_brewer(direction = -1) +
#'   theme_dark()
#' }
#'
#' # Use distiller variant with continuous data
#' v <- ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density))
#' v
#' v + scale_fill_distiller()
#' v + scale_fill_distiller(palette = "Spectral")
#' # the order of colour can be reversed, but with scale_*_distiller(),
#' # the default direction = -1, so to reverse, use direction = 1.
#' v + scale_fill_distiller(palette = "Spectral", direction = 1)
#'
#' # or use blender variants to discretise continuous data
#' v + scale_fill_fermenter()
#'
scale_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "colour") {
  discrete_scale(aesthetics, palette = pal_brewer(type, palette, direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "fill") {
  discrete_scale(aesthetics, palette = pal_brewer(type, palette, direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    palette = pal_gradient_n(pal_brewer(type, palette, direction)(7), values, space),
    na.value = na.value, guide = guide, ...
  )
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}

#' @export
#' @rdname scale_brewer
scale_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") {
  type <- arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    palette = pal_gradient_n(pal_brewer(type, palette, direction)(7), values, space),
    na.value = na.value, guide = guide, ...
  )
}

#' @export
#' @rdname scale_brewer
scale_colour_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a binned scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  binned_scale(aesthetics, palette = pal_binned(pal_brewer(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a binned scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  binned_scale(aesthetics, palette = pal_binned(pal_brewer(type, palette, direction)), na.value = na.value, guide = guide, ...)
}
