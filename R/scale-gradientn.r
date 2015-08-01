#' @inheritParams scales::gradient_n_pal
#' @param colours Vector of colours to use for n-colour gradient.
#' @rdname scale_gradient
#' @export
scale_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("colour", "gradientn",
    gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
#' @rdname scale_gradient
#' @export
scale_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("fill", "gradientn",
    gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
