#' @rdname geom_hline
#' @export
geom_abline <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", show_guide = FALSE, ...) { 
  GeomAbline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomAbline <- proto(Geom, {
  objname <- "abline"

  draw <- function(., data, scales, coordinates, intercept = NULL, slope = NULL, ...) {
    ranges <- coord_range(coordinates, scales)

    if (!is.null(intercept))
      data$intercept <- intercept
    if (!is.null(slope))
      data$slope <- slope

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    if(nrow(data) > 1 && nrow(unique(data)) == 1)
      message("There are ", nrow(data), " identical ablines. If you want just one line, use annontate(\"abline\") instead of geom_abline().")


    GeomSegment$draw(data, scales, coordinates)
  }

  icon <- function(.) linesGrob(c(0, 1), c(0.2, 0.8))
  guide_geom <- function(.) "abline"

  default_stat <- function(.) StatAbline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0, 0, 1, 1, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, lty=linetype,
        lineend="butt")))
    )
  }
})
