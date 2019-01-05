#' draw_key_shadows 
#' draws the legend box for geom_shadows
#' currently stored in 'legend-draw-shadows.r'
#' would usually be stored with other draw_keys in 'legend-draw.r'
#' @export
#' @rdname draw_key
draw_key_shadows <- function(data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0
  segmentsGrob(0.5, 0.1, 0.5, 0.9,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    ),
    arrow = params$arrow
  )
}
