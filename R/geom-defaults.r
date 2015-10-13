#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param stat,geom Name of geom/stat to modify (like \code{"point"} or
#'   \code{"bin"}), or a Geom/Stat object (like \code{GeomPoint} or
#'   \code{StatBin}).
#' @param new Named list of aesthetics.
#' @export
#' @examples
#' update_geom_defaults("point", list(colour = "darkblue"))
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' update_geom_defaults("point", list(colour = "black"))
#' @rdname update_defaults
update_geom_defaults <- function(geom, new) {
  if (is.character(geom)) {
    g <- find_subclass("Geom", geom)
  } else if (inherits(geom, "Geom")) {
    g <- geom
  } else {
    stop('`geom` must be a string (like "point") or a Geom object (like GeomPoint).',
      call. = FALSE)
  }

  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  if (is.character(stat)) {
    g <- find_subclass("Stat", stat)
  } else if (inherits(stat, "Stat")) {
    g <- stat
  } else {
    stop('`stat` must be a string (like "point") or a Stat object (like StatBin).',
      call. = FALSE)
  }

  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}
