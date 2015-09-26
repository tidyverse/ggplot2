#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom name of geom/stat to modify
#' @param new named list of aesthetics
#' @export
#' @examples
#' update_geom_defaults("point", list(colour = "darkblue"))
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' update_geom_defaults("point", list(colour = "black"))
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  g <- find_subclass("Geom", geom)
  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  g <- find_subclass("Stat", stat)
  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}
