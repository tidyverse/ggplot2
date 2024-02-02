#' Create a complete ggplot appropriate to a particular data type
#'
#' `autoplot()` uses ggplot2 to draw a particular plot for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of autoplot
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' @seealso [autolayer()], [ggplot()] and [fortify()]
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}

#' @export
autoplot.default <- function(object, ...) {
  cli::cli_abort(c(
    "Objects of class {.cls {class(object)[[1]]}} are not supported by autoplot.",
    "i" = "Have you loaded the required package?"
  ))
}

