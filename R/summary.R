#' Displays a useful description of a ggplot object
#'
#' @noRd
#' @param object ggplot2 object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @name summary.ggplot
#' @aliases summary.ggplot summary.ggplot2::ggplot
#' @usage summary(object, ...)
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' summary(p)
S7::method(summary, class_ggplot) <- function(object, ...) {
  wrap <- function(x) paste(
    paste(strwrap(x, exdent = 2), collapse = "\n"),
    "\n", sep = ""
    )

  if (!is.null(object@data)) {
    output <- paste(
      "data:     ", paste(names(object@data), collapse = ", "),
      " [", nrow(object@data), "x", ncol(object@data), "] ",
      "\n", sep = "")
    cat(wrap(output))
  }
  if (length(object@mapping) > 0) {
    cat("mapping:  ", clist(object@mapping), "\n", sep = "")
  }
  if (object@scales$n() > 0) {
    cat("scales:  ", paste(object@scales$input(), collapse = ", "), "\n")
  }

  vars <- object@facet$vars()
  vars <- if (length(vars) > 0) paste0("~", vars) else "<empty>"
  cat("faceting: ", paste0(vars, collapse = ", "), "\n")

  if (length(object@layers) > 0)
    cat("-----------------------------------\n")
  invisible(lapply(object@layers, function(x) {
    print(x)
    cat("\n")
  }))

}
