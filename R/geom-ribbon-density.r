GeomDensity <- proto(GeomArea, {
  objname <- "density"
  desc <- "Display a smooth density estimate"
  details <- "A smooth density estimate calculated by stat_density"
  icon <- function(.) {
    x <- seq(0, 1, length=80)
    y <- dnorm(x, mean=0.5, sd=0.15)
    linesGrob(x, 0.05 + y / max(y) * 0.9, default="npc")
  }
  default_stat <- function(.) StatDensity
  default_pos <- function(.) PositionIdentity
  
  seealso <- list(
    geom_histogram = "for the histogram"
  )  

  default_aes <- function(.) plyr::defaults(aes(fill=NA, weight=1, colour="black", alpha = 1), GeomArea$default_aes())

  examples <- function(.) {
    # See stat_density for examples
  }
})
