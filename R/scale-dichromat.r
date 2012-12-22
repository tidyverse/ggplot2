#' Dichromat (colour-blind) scales
#'
#' Color schemes suitable for deficient or anomalous red-green vision.
#' See \pkg{dichromat} for more information.
#'
#' @inheritParams scales::dichromat_pal
#' @inheritParams scale_colour_hue
#' @family colour scales
#' @rdname scale_dichromat
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- qplot(carat, price, data=dsamp, colour=clarity))
#' d + scale_colour_dichromat()
#'
#' # Select dichromat palette to use, see ?scales::dichromat_pal for more details
#' d + scale_colour_dichromat(name="BluetoOrange.8")
#' d + scale_colour_dichromat(name="BluetoGray.8")
#' d + scale_colour_dichromat(name="Categorical.12")
#'
#' # scale_fill_dichromat works just the same as
#' # scale_colour_dichromat but for fill colours
#' ggplot(diamonds, aes(x=price, fill=cut)) +
#'   geom_histogram(position="dodge", binwidth=1000) +
#'   scale_fill_dichromat()
scale_colour_dichromat <- function(..., name="BluetoOrange.10") {
  discrete_scale("colour", "dichromat", dichromat_pal(name), ...)
}

#' @export
#' @rdname scale_dichromat
scale_fill_dichromat <- function(..., name="BluetoOrange.10") {
  discrete_scale("fill", "dichromat", dichromat_pal(name), ...)
}
