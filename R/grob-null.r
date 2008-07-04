.nullGrob <- grob(cl = "nullGrob", name = "NULL")
widthDetails.nullGrob <-
heightDetails.nullGrob <- 
grobWidth.nullGrob <- 
grobHeight.nullGrob <- function(x) unit(0, "cm")

drawDetails.nullGrob <- function(x, recording) {}

nullGrob <- function() .nullGrob
