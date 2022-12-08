geom_defaults_cache <- new.env(parent = emptyenv())
stat_defaults_cache <- new.env(parent = emptyenv())

#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom Name of geom/stat to modify (like `"point"` or
#'   `"bin"`), or a Geom/Stat object (like `GeomPoint` or
#'   `StatBin`).
#' @param new Named list of aesthetics. Alternatively, `NULL` to reset the
#'   defaults.
#' @keywords internal
#' @export
#' @examples
#' update_geom_defaults("point", list(colour = "darkblue"))
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' # Reset defaults by using `new = NULL`
#' update_geom_defaults("point", NULL)
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  if (is.null(new)) {

    vec_assert(geom, character(), 1)
    old <- geom_defaults_cache[[geom]]
    if (!is.null(old)) {
      new <- update_geom_defaults(geom, old)
      env_unbind(geom_defaults_cache, geom)
    }
    invisible(new)

  } else {

    g <- check_subclass(geom, "Geom", env = parent.frame())
    old <- g$default_aes
    # Only update cache the first time
    if (!geom %in% ls(geom_defaults_cache)) {
      geom_defaults_cache[[geom]] <- old
    }
    g$default_aes <- defaults(rename_aes(new), old)
    invisible(old)

  }
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  if (is.null(new)) {

    vec_assert(stat, character(), 1)
    old <- stat_defaults_cache[[stat]]
    if (!is.null(old)) {
      new <- update_geom_defaults(stat, old)
      env_unbind(geom_defaults_cache, stat)
    }
    invisible(new)

  } else {

    g <- check_subclass(stat, "Stat", env = parent.frame())
    old <- g$default_aes
    # Only update cache the first time
    if (!stat %in% ls(stat_defaults_cache)) {
      stat_defaults_cache[[stat]] <- old
    }
    g$default_aes <- defaults(rename_aes(new), old)
    invisible(old)

  }
}
