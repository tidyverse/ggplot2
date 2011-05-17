#' Create your own discrete scale.
#' 
#' @export scale_colour_manual scale_fill_manual scale_shape_manual
#'   scale_linetype_manual scale_alpha_manual scale_size_manual
#' @examples
#' p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
#' 
#' p + scale_colour_manual(values = c("red","blue", "green"))
#' p + scale_colour_manual(
#'   values = c("8" = "red","4" = "blue","6" = "green"))
#' 
#' # As with other scales you can use breaks to control the appearance
#' # of the legend
#' cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
#' p + scale_colour_manual(values = cols)
#' p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
#' p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
#' p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
#'   labels = c("four", "six", "eight"))
#' 
#' # And limits to control the possible values of the scale
#' p + scale_colour_manual(values = cols, limits = c("4", "8"))
#' p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
scale_colour_manual <- function(..., values) {
  discrete_scale("colour", "manual", manual_pal(values))
}
scale_fill_manual <- function(..., values) {
  discrete_scale("fill", "manual", manual_pal(values))
}
scale_size_manual <- function(..., values) {
  discrete_scale("size", "manual", manual_pal(values))
}
scale_shape_manual <- function(..., values) {
  discrete_scale("shape", "manual", manual_pal(values))
}
scale_linetype_manual <- function(..., values) {
  discrete_scale("linetype", "manual", manual_pal(values))
}
scale_alpha_manual <- function(..., values) {
  discrete_scale("alpha", "manual", manual_pal(values))
}

icon.manual <- function() textGrob("DIY", gp=gpar(cex=1.2))

