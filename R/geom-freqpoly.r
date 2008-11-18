GeomFreqpoly <- proto(Geom, {
  objname <- "freqpoly"
  desc <- "Frequency polygon"
  icon <- function(.) {
    y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
    linesGrob(seq(0.1, 0.9, by=0.1), y, gp=gpar(col="grey20"))
  }
  
  default_aes <- function(.) GeomPath$default_aes()
  default_stat <- function(.) StatBin
  draw <- function(., ...) GeomPath$draw(...)
  
  
  seealso <- list(
    geom_histogram = GeomHistogram$desc
  )
  
  examples <- function(.) {
    qplot(carat, data = diamonds, geom="freqpoly")
    qplot(carat, data = diamonds, geom="freqpoly", binwidth = 0.1)
    qplot(carat, data = diamonds, geom="freqpoly", binwidth = 0.01)

    qplot(price, data = diamonds, geom="freqpoly", binwidth = 1000)
    qplot(price, data = diamonds, geom="freqpoly", binwidth = 1000, 
      colour = color)
    qplot(price, ..density.., data = diamonds, geom="freqpoly", 
      binwidth = 1000, colour = color)

  }
})
