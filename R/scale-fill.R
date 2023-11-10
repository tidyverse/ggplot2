#' @include scale-brewer.R scale-colour-ordinal.R scale-colour.R scale-gradient.R scale-grey.R scale-hue.R scale-identity.R scale-manual.R scale-steps.R scale-viridis.R
NULL


## general color scales ---------------------------

#' @export
#' @rdname scale_colour_continuous
scale_fill_binned <- reuse_scale(scale_colour_binned, "fill")

#' @export
#' @rdname scale_colour_continuous
scale_fill_continuous <- reuse_scale(scale_colour_continuous, "fill")

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_datetime <- reuse_scale(scale_colour_datetime, "fill")

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_date <- reuse_scale(scale_colour_date, "fill")

#' @export
#' @rdname scale_colour_discrete
scale_fill_discrete <- reuse_scale(scale_colour_discrete, "fill")

#' @export
#' @rdname scale_identity
scale_fill_identity <- reuse_scale(scale_colour_identity, "fill")

#' @export
#' @rdname scale_manual
scale_fill_manual <- reuse_scale(scale_colour_manual, "fill")

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_fill_ordinal <- reuse_scale(scale_colour_ordinal, "fill")

scale_fill_qualitative <- reuse_scale(scale_colour_qualitative, "fill")


## specific color scales --------------------------

#' @export
#' @rdname scale_brewer
scale_fill_brewer <- reuse_scale(scale_colour_brewer, "fill")

#' @export
#' @rdname scale_brewer
scale_fill_distiller <- reuse_scale(scale_colour_distiller, "fill")

#' @export
#' @rdname scale_brewer
scale_fill_fermenter <- reuse_scale(scale_colour_fermenter, "fill")

#' @export
#' @rdname scale_gradient
scale_fill_gradient <- reuse_scale(scale_colour_gradient, "fill")

#' @export
#' @rdname scale_gradient
scale_fill_gradient2 <- reuse_scale(scale_colour_gradient2, "fill")

#' @export
#' @rdname scale_gradient
scale_fill_gradientn <- reuse_scale(scale_colour_gradientn, "fill")

#' @export
#' @rdname scale_grey
scale_fill_grey <- reuse_scale(scale_colour_grey, "fill")

#' @export
#' @rdname scale_hue
scale_fill_hue <- reuse_scale(scale_colour_hue, "fill")

#' @export
#' @rdname scale_steps
scale_fill_steps <- reuse_scale(scale_colour_steps, "fill")

#' @export
#' @rdname scale_steps
scale_fill_steps2 <- reuse_scale(scale_colour_steps2, "fill")

#' @export
#' @rdname scale_steps
scale_fill_stepsn <- reuse_scale(scale_colour_stepsn, "fill")

#' @export
#' @rdname scale_viridis
scale_fill_viridis_b <- reuse_scale(scale_colour_viridis_b, "fill")

#' @export
#' @rdname scale_viridis
scale_fill_viridis_c <- reuse_scale(scale_colour_viridis_c, "fill")

#' @export
#' @rdname scale_viridis
scale_fill_viridis_d <- reuse_scale(scale_colour_viridis_d, "fill")
