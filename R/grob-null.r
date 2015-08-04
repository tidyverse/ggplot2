#' The zero grob draws nothing and has zero size.
#'
#' @keywords internal
#' @export
zeroGrob <- function() .zeroGrob

.zeroGrob <- grob(cl = "zeroGrob", name = "NULL")
#' @export
#' @method widthDetails zeroGrob
widthDetails.zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method heightDetails zeroGrob
heightDetails.zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method grobWidth zeroGrob
grobWidth.zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method grobHeight zeroGrob
grobHeight.zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method drawDetails zeroGrob
drawDetails.zeroGrob <- function(x, recording) {}

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")
