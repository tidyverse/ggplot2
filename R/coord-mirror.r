#' Cartesian coordinates with R to L mapping
#'
#' Flip cartesian coordinates so that the y-axis is plotted on the right hand
#' side. This is primarily useful for plotting in
#' right to left languages.
#'
#' @export
#' @inheritParams coord_cartesian

coord_mirror <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordMirror,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordMirror <- ggproto("CoordMirror", CoordCartesian,
  modify_scales = function(scales_x, scales_y) {
    lapply(scales_y, scale_flip_position)
  }
)
