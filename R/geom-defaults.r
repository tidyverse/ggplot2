#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom Name of geom/stat to modify (like `"point"` or
#'   `"bin"`), or a Geom/Stat object (like `GeomPoint` or
#'   `StatBin`).
#' @param new Named list of aesthetics.
#' @keywords internal
#' @export
#' @examples
#' update_geom_defaults("point", list(colour = "darkblue"))
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' update_geom_defaults("point", list(colour = "black"))
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  g <- check_subclass(geom, "Geom", env = parent.frame())
  old <- g$default_aes
  g$default_aes <- defaults(rename_aes(new), old)
  invisible()
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  g <- check_subclass(stat, "Stat", env = parent.frame())
  old <- g$default_aes
  g$default_aes <- defaults(rename_aes(new), old)
  invisible()
}
