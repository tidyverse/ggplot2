#' Remove duplicates.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "unique")}
#'
#' @export
#' @inheritParams stat_identity
#' @examples
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
#' ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")
stat_unique <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", ...) {
  StatUnique$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatUnique <- proto(Stat, {
  objname <- "unique"
  desc <- "Remove duplicates"

  default_geom <- function(.) GeomPoint

  calculate_groups <- function(., data, scales, ...) unique(data)
})
