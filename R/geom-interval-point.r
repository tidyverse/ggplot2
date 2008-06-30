GeomPointrange <- proto(GeomInterval, {
  objname <- "pointrange"
  desc <- "An interval represented by a vertical line, with a point in the middle"
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
      pointsGrob(c(0.3, 0.7), c(0.4, 0.6), pch=19, gp=gpar(col="black", cex=0.5), default.units="npc")
    ))
  }
  
  seealso <- list(
    "geom_errorbar" = "error bars",
    "geom_linerange" = "range indicated by straight line, + examples",
    "geom_crossbar" = "hollow bar with middle indicated by horizontal line",
    "stat_summary " = "examples of these guys in use",
    "geom_smooth" = "for continuous analog"
  )
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=1, linetype=1, shape=19, fill=NA)
  guide_geom <- function(.) "pointrange"

  draw <- function(., data, scales, coordinates, ...) {
    if (is.null(data$y)) return(GeomLinerange$draw(data, scales, coordinates, ...))
    ggname(.$my_name(),gTree(children=gList(
      GeomLinerange$draw(transform(data, size = size / 2), scales, coordinates, ...),
      GeomPoint$draw(data, scales, coordinates, ...)
    )))
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    gTree(children = gList(
      GeomPath$draw_legend(data, ...),
      GeomPoint$draw_legend(data, ...)
    ))
  }
  
  
  examples <- function(.) {
    # See geom_linerange for examples
  }
  
})
