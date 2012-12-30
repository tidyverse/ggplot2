#' The zero grob draws nothing and has zero size.
#' 
#' @S3method widthDetails zeroGrob
#' @S3method heightDetails zeroGrob
#' @S3method grobWidth zeroGrob
#' @S3method grobHeight zeroGrob
#' @S3method drawDetails zeroGrob
#' @keywords internal
zeroGrob <- function() .zeroGrob

.zeroGrob <- grob(cl = "zeroGrob", name = "NULL")
widthDetails.zeroGrob <-
heightDetails.zeroGrob <- 
grobWidth.zeroGrob <- 
grobHeight.zeroGrob <- function(x) unit(0, "cm")

drawDetails.zeroGrob <- function(x, recording) {}

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")
