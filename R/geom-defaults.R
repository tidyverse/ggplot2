#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom Name of geom/stat to modify (like `"point"` or
#'   `"bin"`), or a Geom/Stat object (like `GeomPoint` or
#'   `StatBin`).
#' @param new Named list of aesthetics.
#' @keywords internal
#' @export
#' @examples
#'
#' # updating a geom's default aesthetic settings
#' # example: change geom_point()'s default color
#' GeomPoint$default_aes
#' update_geom_defaults("point", aes(color = "red"))
#' GeomPoint$default_aes
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # reset default
#' update_geom_defaults("point", aes(color = "black"))
#'
#'
#' # updating a stat's default aesthetic settings
#' # example: change stat_bin()'s default y-axis to the density scale
#' StatBin$default_aes
#' update_stat_defaults("bin", aes(y = after_stat(density)))
#' StatBin$default_aes
#' ggplot(data.frame(x = rnorm(1e3)), aes(x)) +
#'   geom_histogram() +
#'   geom_function(fun = dnorm, color = "red")
#'
#' # reset default
#' update_stat_defaults("bin", aes(y = after_stat(count)))
#'
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  g <- check_subclass(geom, "Geom", env = parent.frame())
  old <- g$default_aes
  new <- rename_aes(new)
  new_names_order <- unique(c(names(old), names(new)))
  new <- defaults(new, old)[new_names_order]
  g$default_aes[names(new)] <- new
  invisible()
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  g <- check_subclass(stat, "Stat", env = parent.frame())
  old <- g$default_aes
  new <- rename_aes(new)
  new_names_order <- unique(c(names(old), names(new)))
  new <- defaults(new, old)[new_names_order]
  g$default_aes[names(new)] <- new
  invisible()
}
