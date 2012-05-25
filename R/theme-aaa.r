#' Relative sizing for theme elements
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @S3method print rel
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' Reports whether x is a rel object
is.rel <- function(x) inherits(x, "rel")
