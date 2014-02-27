#' Displays a useful description of a ggplot object
#'
#' @param object ggplot2 object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary ggplot
#' @export
#' @examples
#' summary(qplot(mpg, wt, data=mtcars))
summary.ggplot <- function(object, ...) {
  wrap <- function(x) paste(
    paste(strwrap(x, exdent = 2), collapse = "\n"),
    "\n", sep =""
    )

  defaults <- function() {
    paste(mapply(function(x, n) {
      paste(n, deparse(x), sep="=")
    }, object$mapping, names(object$mapping)), collapse=", ")
  }

  # cat("Title:    ", object$title, "\n", sep="")
  # cat("-----------------------------------\n")
  if (!is.null(object$data)) {
    output <- paste(
      "data:     ", paste(names(object$data), collapse=", "),
      " [", nrow(object$data), "x", ncol(object$data), "] ",
      "\n", sep="")
    cat(wrap(output))
  }
  if (length(object$mapping) > 0) {
    cat("mapping:  ", clist(object$mapping), "\n", sep="")
  }
  if (object$scales$n() > 0) {
    cat("scales:  ", paste(object$scales$input(), collapse = ", "), "\n")
  }

  cat("faceting: ")
  print(object$facet)

  if (length(object$layers) > 0)
    cat("-----------------------------------\n")
  invisible(lapply(object$layers, function(x) {print(x); cat("\n")}))

}
