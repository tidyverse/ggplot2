#' Modify geom/stat aesthetic defaults for future plots
#'
#' Functions to update or reset the default aesthetics of geoms and stats.
#'
#' @param stat,geom Name of geom/stat to modify (like `"point"` or
#'   `"bin"`), or a Geom/Stat object (like `GeomPoint` or
#'   `StatBin`).
#' @param new One of the following:
#'  * A named list of aesthetics to serve as new defaults.
#'  * `NULL` to reset the defaults.
#' @keywords internal
#' @note
#' Please note that geom defaults can be set *en masse* via the `theme(geom)`
#' argument.
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
#' # reset single default
#' update_geom_defaults("point", NULL)
#'
#' # reset all defaults
#' reset_geom_defaults()
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
#' # reset single default
#' update_stat_defaults("bin", NULL)
#'
#' # reset all defaults
#' reset_stat_defaults()
#'
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  update_defaults(geom, "Geom", new, env = parent.frame())
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  update_defaults(stat, "Stat", new, env = parent.frame())
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
#'
#' # Changed theme
#' get_geom_defaults("point", theme(geom = element_geom(ink = "purple")))
get_geom_defaults <- function(geom, theme = theme_get()) {
  theme <- theme %||% list(geom = .default_geom_element)

  if (is.function(geom)) {
    geom <- geom()
  }
  if (is.layer(geom)) {
    data <- data_frame0(.id = 1L)
    data <- geom$compute_geom_2(data = data, theme = theme)
    data$.id <- NULL
    return(data)
  }
  if (is.character(geom)) {
    geom <- check_subclass(geom, "Geom")
  }
  if (is.geom(geom)) {
    out <- geom$use_defaults(data = NULL, theme = theme)
    return(out)
  }
  stop_input_type(geom, as_cli("a layer function, string or {.cls Geom} object"))
}

#' @rdname update_defaults
#' @export
reset_geom_defaults <- function() reset_defaults("geom")

#' @rdname update_defaults
#' @export
reset_stat_defaults <- function() reset_defaults("stat")

cache_defaults <- new_environment()

update_defaults <- function(name, subclass, new, env = parent.frame()) {
  obj   <- check_subclass(name, subclass, env = env)
  index <- snake_class(obj)

  if (is.null(new)) { # Reset from cache

    old <- cache_defaults[[index]]
    if (!is.null(old)) {
      new <- update_defaults(name, subclass, new = old, env = env)
    }
    invisible(new)

  } else { # Update default aesthetics

    old <- obj$default_aes
    # Only update cache the first time defaults are changed
    if (!exists(index, envir = cache_defaults)) {
      cache_defaults[[index]] <- old
    }
    new <- rename_aes(new)
    name_order <- unique(c(names(old), names(new)))
    new <- defaults(new, old)[name_order]
    obj$default_aes[names(new)] <- new
    invisible(old)

  }
}

reset_defaults <- function(type) {
  # Lookup matching names in cache
  prefix <- paste0("^", type, "_")
  full_names <- grep(prefix, ls(cache_defaults), value = TRUE)
  # Early exit if there is nothing to reset
  if (length(full_names) < 1) {
    return(invisible())
  }
  # Format names without prefix
  short_names <- gsub(prefix, "", full_names)
  names(short_names) <- full_names

  # Run updates
  update <- switch(type, geom = update_geom_defaults, update_stat_defaults)
  invisible(lapply(short_names, update, new = NULL))
}
