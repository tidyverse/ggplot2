#' @inheritParams scales::div_gradient_pal
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'   Defaults to 0.
#' @rdname scale_gradient
#' @export
scale_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("colour", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_gradient
#' @export
scale_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") {
  continuous_scale("fill", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}
