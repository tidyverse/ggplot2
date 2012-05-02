#' @rdname geom_hline
#' @export
geom_vline <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", show_guide = FALSE, ...) { 
  GeomVline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomVline <- proto(Geom, {
  objname <- "vline"

  draw <- function(., data, scales, coordinates, xintercept = NULL, ...) {
    ranges <- coord_range(coordinates, scales)

    data$x    <- xintercept %||% data$xintercept
    data$xend <- data$x
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]
    
    if(nrow(data) > 1 && nrow(unique(data)) == 1)
      message(nrow(data), " identical vlines were drawn. If you want just one line, use annontate(\"vline\") instead of geom_vline().")

    GeomSegment$draw(data, scales, coordinates)
  }

  
  icon <- function(.) linesGrob(c(0.5, 0.5), c(0, 1))
  default_stat <- function(.) StatVline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "vline"

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0.5, 0, 0.5, 1, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }

})
