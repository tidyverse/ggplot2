#' @export
#'   scale_colour_discrete
#'   scale_colour_continuous
#'   scale_fill_discrete
#'   scale_fill_continuous
#'   scale_color_brewer
#'   scale_color_continuous
#'   scale_color_discrete
#'   scale_color_gradient
#'   scale_color_grey
#'   scale_color_hue
#'   scale_color_identity
#'   scale_color_manual
#'   coord_equal
NULL

scale_colour_discrete <- scale_colour_hue
scale_colour_continuous <- scale_colour_gradient
scale_fill_discrete <- scale_fill_hue
scale_fill_continuous <- scale_fill_gradient

# British to American spellings
scale_color_brewer <- scale_colour_brewer
scale_color_continuous <- scale_colour_gradient
scale_color_discrete <- scale_colour_hue
scale_color_gradient <- scale_colour_gradient
# scale_color_gradient2 <- scale_colour_gradient2
# scale_color_gradientn <- scale_colour_gradientn
scale_color_grey <- scale_colour_grey
scale_color_hue <- scale_colour_hue
scale_color_identity <- scale_colour_identity
scale_color_manual <- scale_colour_manual

# Old names
coord_equal <- coord_fixed
