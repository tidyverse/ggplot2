#' Qualitative colour scale with evenly spaced hues.
#' 
#' @export scale_colour_hue scale_fill_hue
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- qplot(carat, price, data=dsamp, colour=clarity))
#' 
#' # Change scale label
#' d + scale_colour_hue()
#' d + scale_colour_hue("clarity")
#' d + scale_colour_hue(expression(clarity[beta]))
#' 
#' # Adjust luminosity and chroma
#' d + scale_colour_hue(l=40, c=30)
#' d + scale_colour_hue(l=70, c=30)
#' d + scale_colour_hue(l=70, c=150)
#' d + scale_colour_hue(l=80, c=150)
#' 
#' # Change range of hues used
#' d + scale_colour_hue(h=c(0, 90))
#' d + scale_colour_hue(h=c(90, 180))
#' d + scale_colour_hue(h=c(180, 270))
#' d + scale_colour_hue(h=c(270, 360))
#' 
#' # Vary opacity
#' # (only works with pdf, quartz and cairo devices)
#' d <- ggplot(dsamp, aes(carat, price, colour = clarity))
#' d + geom_point(alpha = 0.9)
#' d + geom_point(alpha = 0.5)
#' d + geom_point(alpha = 0.2)
scale_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1) {
  discrete_scale("colour", "hue", hue_pal(h, c, l, h.start, direction))
}

scale_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1) {
  discrete_scale("fill", "hue", hue_pal(h, c, l, h.start, direction))
}

icon.hue <- function() {
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
    gp=gpar(fill=hcl(seq(0, 360, length=6)[-6], c=100, l=65), col=NA)
  )
}
