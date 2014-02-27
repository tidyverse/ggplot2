#' Plot templates.
#'
#' These plot templates are deprecated in an attempt to make ggplot2 as
#' streamlined as possible, and to avoid bugs in these poorly tested
#' functions. See the \pkg{GGally} package for some alternatives.
#'
#' @name plot-templates
#' @keywords internal
NULL

#' @export
#' @rdname plot-templates
ggpcp <- function(data, vars=names(data), ...) {
  gg_dep("0.9.1", "ggpcp is deprecated.")
}

#' @export
#' @rdname plot-templates
ggfluctuation <- function(table, type="size", floor=0, ceiling=max(table$freq, na.rm=TRUE)) {
  gg_dep("0.9.1", "ggfluctuation is deprecated.")
}

#' @export
#' @rdname plot-templates
ggmissing <- function(data, avoid="stack", order=TRUE, missing.only = TRUE) {
  gg_dep("0.9.1", "ggmissing is deprecated.")
}

#' @export
#' @rdname plot-templates
ggstructure <- function(data) {
  gg_dep("0.9.1", "ggstructure is deprecated.")
}

#' @export
#' @rdname plot-templates
ggorder <- function(data) {
  gg_dep("0.9.1", "ggorder is deprecated.")
}

#' @export
#' @rdname plot-templates
plotmatrix <- function(data, mapping=aes(), colour="black") {
  gg_dep("0.9.2", "This function is deprecated. For a replacement, see the ggpairs function in the GGally package.")
}

