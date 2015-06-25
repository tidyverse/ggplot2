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
  # R6 TODO: Avoid instantiation. This will not work currently.
  g <- Geom$new()$find(geom)$new()
  old <- g$default_aes()

  aes <- defaults(new, old)

  # R6 TODO: This is is a workaround until we figure out a way to do singletons
  # g$default_aes <- eval(substitute(function() aes, list(aes = aes)))
  Geom$new()$find(geom)$set("public", "default_aes",
    eval(substitute(function() aes, list(aes = aes))),
    overwrite = TRUE
  )
}

#' @rdname update_defaults
#' @export
update_stat_defaults <- function(stat, new) {
  # R6 TODO: Avoid instantiation. This will not work currently.
  g <- Stat$new()$find(stat)$new()
  old <- g$default_aes()

  aes <- defaults(new, old)
  # g$default_aes <- eval(substitute(function() aes, list(aes = aes)))
  # R6 TODO: This is is a workaround until we figure out a way to do singletons
  Stat$new()$find(stat)$set("public", "default_aes",
    eval(substitute(function() aes, list(aes = aes))),
    overwrite = TRUE
  )
}
