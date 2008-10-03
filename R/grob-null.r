# Null grob
# The null grob does nothing and has zero size.
# 
# @alias widthDetails.nullGrob
# @alias heightDetails.nullGrob
# @alias grobWidth.nullGrob
# @alias grobHeight.nullGrob
# @alias drawDetails.nullGrob
# @keyword internal
nullGrob <- function() .nullGrob

.nullGrob <- grob(cl = "nullGrob", name = "NULL")
widthDetails.nullGrob <-
heightDetails.nullGrob <- 
grobWidth.nullGrob <- 
grobHeight.nullGrob <- function(x) unit(0, "cm")

drawDetails.nullGrob <- function(x, recording) {}

