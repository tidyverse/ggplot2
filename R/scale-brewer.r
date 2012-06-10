#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' Create colour scales based on ColorBrewer colours.
#' 
#' ColorBrewer provides sequential, diverging and qualitative colour schemes
#' which are particularly suited and tested to display discrete values (levels
#' of a factor) on a map. ggplot2 can use those colours in discrete scales. It
#' also allows to smoothly interpolate the colours to a continuous scale,
#' although the original colour schemes (particularly the qualitative ones)
#' were not intended for this. The perceptual result is left to the
#' appreciation of the user.
#'
#' See \url{http://colorbrewer2.org} for more information.
#'
#' @inheritParams scales::brewer_pal
#' @inheritParams scale_colour_hue
#' @inheritParams scale_colour_gradient
#' @family colour scales
#' @rdname scale_brewer
#' @export 
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- qplot(carat, price, data=dsamp, colour=clarity))
#' 
#' # Change scale label
#' d + scale_colour_brewer()
#' d + scale_colour_brewer("clarity")
#' d + scale_colour_brewer(expression(clarity[beta]))
#' 
#' # Select brewer palette to use, see ?scales::brewer_pal for more details
#' d + scale_colour_brewer(type="seq")
#' d + scale_colour_brewer(type="seq", palette=3)
#' 
#' d + scale_colour_brewer(palette="Blues")
#' d + scale_colour_brewer(palette="Set1")
#' 
#' # scale_fill_brewer works just the same as 
#' # scale_colour_brewer but for fill colours
#' ggplot(diamonds, aes(x=price, fill=cut)) + 
#'   geom_histogram(position="dodge", binwidth=1000) + 
#'   scale_fill_brewer()
scale_colour_brewer <- function(..., type = "seq", palette = 1) {
  discrete_scale("colour", "brewer", brewer_pal(type, palette), ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_brewer <- function(..., type = "seq", palette = 1) {
  discrete_scale("fill", "brewer", brewer_pal(type, palette), ...)
}

#' @export
#' @rdname scale_brewer
scale_colour_distiller <- function(..., type = "seq", palette = 1, values = NULL, space = "Lab", na.value = "grey50") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type=\"seq\" or type=\"div\" instead")
  }
  continuous_scale("colour", "distiller",
    gradient_n_pal(brewer_pal(type, palette)(6), values, space), na.value = na.value, ...)
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}

#' @export
#' @rdname scale_brewer
scale_fill_distiller <- function(..., type = "seq", palette = 1, values = NULL, space = "Lab", na.value = "grey50") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type=\"seq\" or type=\"div\" instead")
  }
  continuous_scale("fill", "distiller",
    gradient_n_pal(brewer_pal(type, palette)(6), values, space), na.value = na.value, ...)
}
