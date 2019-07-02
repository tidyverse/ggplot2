
#' Empty guide
#'
#' This guide draws nothing.
#'
#' @export
#'
guide_none <- function() {
  structure(list(available_aes = "any"), class = c("guide", "guide_none"))
}

#' @export
guide_train.guide_none <- function(guide, scale, aesthetic = NULL) {
  guide
}

#' @export
guide_merge.guide_none <- function(guide, new_guide) {
  guide
}

#' @export
guide_geom.guide_none <- function(guide, layers, default_mapping) {
  guide
}

#' @export
guide_gengrob.guide_none <- function(guide, theme, ...) {
  zeroGrob()
}
