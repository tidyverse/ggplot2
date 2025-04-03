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

#' Resolve and get geom defaults
#'
#' @param geom Some definition of a geom:
#' * A `function` that creates a layer, e.g. `geom_path()`.
#' * A layer created by such function
#' * A string naming a geom class in snake case without the `geom_`-prefix,
#'   e.g. `"contour_filled"`.
#' * A geom class object.
#' @param theme A [`theme`] object. Defaults to the current global theme.
#'
#' @return A list of aesthetics
#' @export
#' @keywords internal
#'
#' @examples
#' # Using a function
#' get_geom_defaults(geom_raster)
#'
#' # Using a layer includes static aesthetics as default
#' get_geom_defaults(geom_tile(fill = "white"))
#'
#' # Using a class name
#' get_geom_defaults("density_2d")
#'
#' # Using a class
#' get_geom_defaults(GeomPoint)
get_geom_defaults <- function(geom, theme = theme_get()) {
  theme <- theme %||% list()

  if (is.function(geom)) {
    geom <- geom()
  }
  if (is_layer(geom)) {
    data <- data_frame0(.id = 1L)
    data <- geom$compute_geom_2(data = data)
    data$.id <- NULL
    return(data)
  }
  if (is.character(geom)) {
    geom <- check_subclass(geom, "Geom")
  }
  if (inherits(geom, "Geom")) {
    out <- geom$use_defaults(data = NULL)
    return(out)
  }
  stop_input_type(geom, as_cli("a layer function, string or {.cls Geom} object"))
}
