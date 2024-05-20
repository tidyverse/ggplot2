
#' The previous S3 guide system
#'
#' The guide system has been overhauled to use the ggproto infrastructure to
#' accommodate guide extensions with the same flexibility as layers, scales and
#' other ggplot2 objects. In rewriting, the old S3 system has become defunct,
#' meaning that the previous methods for guides have been superseded by ggproto
#' methods. As a fallback option, the generics, but not the methods, that the
#' previous S3 system used are encapsulated in the `GuideOld` ggproto class.
#'
#' @param guide An old guide object
#' @keywords internal
#' @name old_guide

#' @export
#' @rdname old_guide
guide_train <- function(guide, scale, aesthetic = NULL) {
  UseMethod("guide_train")
}

#' @export
guide_train.default <- function(guide, ...) {
  cli::cli_abort(c(
    "{.cls Guide} classes have been rewritten as {.cls ggproto} classes.",
    "The old S3 guide methods have been superseded."
  ))
}

#' @export
#' @rdname old_guide
guide_merge <- function(guide, new_guide) {
  UseMethod("guide_merge")
}

#' @export
guide_merge.default <- guide_train.default

#' @export
#' @rdname old_guide
guide_geom <- function(guide, layers, default_mapping = NULL) {
  UseMethod("guide_geom")
}

#' @export
guide_geom.default <- guide_train.default

#' @export
#' @rdname old_guide
guide_transform <- function(guide, coord, panel_params) {
  UseMethod("guide_transform")
}

#' @export
guide_transform.default <- guide_train.default

#' @export
#' @rdname old_guide
guide_gengrob <- function(guide, theme) {
  UseMethod("guide_gengrob")
}

#' @export
guide_gengrob.default <- guide_train.default

#' @export
#' @rdname old_guide
old_guide <- function(guide) {
  deprecate_soft0(
    when = "3.5.0",
    what = I("The S3 guide system"),
    details = c(
      i = "It has been replaced by a ggproto system that can be extended."
    )
  )

  ggproto(
    NULL, GuideOld,
    params = guide,
    available_aes = guide$available_aes %||% NULL
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideOld <- ggproto(
  "GuideOld", Guide,

  train = function(self, params, scale, aesthetic = NULL,
                   title = waiver(), direction = NULL) {
    params$title <- scale$make_title(params$title %|W|% scale$name %|W|% title)
    params$direction <- params$direction %||% direction %||% "vertical"
    params <- guide_train(params, scale, aesthetic)
    params
  },

  merge = function(self, params, new_guide, new_params) {
    guide_merge(params, new_params)
  },

  transform = function(self, params, coord, panel_params, ...) {
    guide_transform(params, coord, panel_params)
  },

  process_layers = function(self, params, layers, data = NULL) {
    guide_geom(params, layers, default_mapping = NULL)
  },

  draw = function(self, theme, position = NULL, direction = NULL, params) {
    params$direction <- params$direction %||% direction %||% "placeholder"
    params$title.position <- params$title.position %||% switch(
      params$direction,
      vertical = "top", horizontal = "left",
      NULL
    )
    guide_gengrob(params, theme)
  }
)

