#' The zero grob draws nothing and has zero size.
#'
#' @keywords internal
#' @export
zeroGrob <- function() .zeroGrob

.zeroGrob <- NULL
on_load(.zeroGrob <- add_class(nullGrob(), "zeroGrob"))

is.zero <- function(x) is.null(x) || inherits(x, "null")
