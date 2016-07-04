# Default scales -------------------------------------------------------------

#' @export
#' @rdname scale_hue
#' @usage NULL
scale_colour_discrete <- scale_colour_hue

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_continuous <- scale_colour_gradient

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_datetime <- function() {
  scale_colour_continuous(trans = "time")
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_date <- function() {
  scale_colour_continuous(trans = "date")
}

#' @export
#' @rdname scale_hue
#' @usage NULL
scale_fill_discrete <- scale_fill_hue

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_continuous <- scale_fill_gradient

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_datetime <- function() {
  scale_fill_continuous(trans = "time")
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_date <- function() {
  scale_fill_continuous(trans = "date")
}


# British to American spellings ----------------------------------------------

#' @export
#' @rdname scale_brewer
#' @usage NULL
scale_color_brewer <- scale_colour_brewer

#' @export
#' @rdname scale_brewer
#' @usage NULL
scale_color_distiller <- scale_colour_distiller

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_continuous <- scale_colour_gradient

#' @export
#' @rdname scale_hue
#' @usage NULL
scale_color_discrete <- scale_colour_hue

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradient <- scale_colour_gradient

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradient2 <- scale_colour_gradient2

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradientn <- scale_colour_gradientn

#' @export
#' @rdname scale_grey
#' @usage NULL
scale_color_grey <- scale_colour_grey

#' @export
#' @rdname scale_hue
#' @usage NULL
scale_color_hue <- scale_colour_hue

#' @export
#' @rdname scale_identity
#' @usage NULL
scale_color_identity <- scale_colour_identity

#' @export
#' @rdname scale_manual
#' @usage NULL
scale_color_manual <- scale_colour_manual
