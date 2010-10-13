GeomQuantile <- proto(GeomPath, {
  objname <- "quantile"
  desc <- "Add quantile lines from a quantile regression"
  
  advice <- "<p>This can be used as a continuous analogue of a geom_boxplot.</p>\n"
  default_stat <- function(.) StatQuantile
  default_aes <- function(.) defaults(aes(weight=1, colour="#3366FF", size=0.5), GeomPath$default_aes())
  guide_geom <- function(.) "path"
  

  icon <- function(.) {
    ggname(.$my_name(), gTree(children=gList(
      linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.8, 0.65, 0.6, 0.6, 0.8)),
      linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.55, 0.45, 0.5, 0.45, 0.55)),
      linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.3, 0.25, 0.4, 0.3, 0.2))
    )))
  }


  examples <- function(.) {
    # See stat_quantile for examples
  }
})
