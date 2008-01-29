GeomCrossbar <- proto(GeomInterval, {
  objname <- "crossbar"
  desc <- "Hollow bar with middle indicated by horizontal line"
  desc_params <- list(
    "fatten" = "a multiplicate factor to fatten middle bar by"
  )

  icon <- function(.) {
    gTree(children=gList(
      rectGrob(c(0.3, 0.7), c(0.6, 0.8), width=0.3, height=c(0.4, 0.4), vjust=1),
      segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
    ))
  }

  seealso <- list(
    "geom_errorbar" = "error bars",
    "geom_pointrange" = "range indicated by straight line, with point in the middle",
    "geom_linerange" = "range indicated by straight line + examples",
    "stat_summary " = "examples of these guys in use",
    "geom_smooth" = "for continuous analog"
  )

  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes = function(.) aes(colour="black", fill="NA", width=resolution(x) * 0.9, size=1, linetype=1, min=y, max=y)
  
  draw <- function(., data, scales, coordinates, fatten = 2, ...) {
    ggname(.$my_name(), gTree(children=gList(
      GeomBar$draw(data, scales, coordinates, ...),
      GeomBar$draw(transform(data, min=y, max=y, size = size * fatten), scales, coordinates, ...)
    )))
  }
  
  examples <- function(.) {
    # See geom_linerange for examples
  }
})


