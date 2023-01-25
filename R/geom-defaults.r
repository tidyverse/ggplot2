cache_geom_defaults <- new.env(parent = emptyenv())
cache_stats_defaults <- new.env(parent = emptyenv())

#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom Name of geom/stat to modify (like `"point"` or `"bin"`),
#'   or a Geom/Stat object (like `GeomPoint` or `StatBin`).
#' @param new Named list of aesthetics. Alternatively, `NULL` to reset the
#'   defaults.
#' @keywords internal
#' @name update_defaults
#' @export
#' @examples
#' # updating a geom's default aesthetic settings
#' # example: change geom_point()'s default color
#' GeomPoint$default_aes
#' update_geom_defaults("point", aes(color = "red"))
#' GeomPoint$default_aes
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # Reset defaults by using `new = NULL`
#' update_geom_defaults("point", NULL)
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
#' update_stat_defaults("bin", NULL)

#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  if (is.null(new)) {

    vec_assert(geom, character(), 1)
    old <- cache_geom_defaults[[geom]]
    if (!is.null(old)) {
      new <- update_geom_defaults(geom, old)
      env_unbind(cache_geom_defaults, geom)
    }
    invisible(new)

  } else {

    g <- check_subclass(geom, "Geom", env = parent.frame())
    old <- g$default_aes
    # Only update cache the first time
    if (!geom %in% ls(cache_geom_defaults)) {
      cache_geom_defaults[[geom]] <- old
    }
    new <- rename_aes(new)
    new_names_order <- unique(c(names(old), names(new)))
    new <- defaults(new, old)[new_names_order]
    g$default_aes[names(new)] <- new
    invisible(old)

  }
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  if (is.null(new)) {

    vec_assert(stat, character(), 1)
    old <- cache_stats_defaults[[stat]]
    if (!is.null(old)) {
      new <- update_stat_defaults(stat, old)
      env_unbind(cache_stats_defaults, stat)
    }
    invisible(new)

  } else {

    g <- check_subclass(stat, "Stat", env = parent.frame())
    old <- g$default_aes
    # Only update cache the first time
    if (!stat %in% ls(cache_stats_defaults)) {
      cache_stats_defaults[[stat]] <- old
    }
    new <- rename_aes(new)
    new_names_order <- unique(c(names(old), names(new)))
    new <- defaults(new, old)[new_names_order]
    g$default_aes[names(new)] <- new
    invisible(old)

  }
}
