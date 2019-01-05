#' draw_key_ellipse 
#' draws the legend box for geom_shadows
#' currently stored in 'legend-draw-ellipse.r'
#' would usually be stored with other draw_keys in 'legend-draw.r'
#' @export
#' @rdname draw_key
draw_key_ellipse <- function(data, params, size) {
  grobTree(
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}
