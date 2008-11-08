GeomFreqpoly <- proto(GeomPath, {
  objname <- "freqpoly"
  desc <- "Frequency polygon"
  icon <- function(.) {
    y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
    linesGrob(seq(0.1, 0.9, by=0.1), y, gp=gpar(col="grey20"))
  }
  
  default_stat <- function(.) StatBin
  
  seealso <- list(
    geom_histogram = GeomHistogram$desc
  )
  
  examples <- function(.) {
  }
})
