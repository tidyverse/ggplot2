#' @include stat-.r
NULL

#' @export
#' @rdname geom_abline
geom_vline <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping is going to be overwritten
    if (!missing(xintercept) && !missing(mapping)) {
      warning(paste0("Using both `xintercept` and `mapping` may not have the desired result as mapping is overwritten if `xintercept` is specified\n",
                     "  Consider placing `xintercept` inside your `aes()` call.\n",
                     "  e.g. `aes(xintercept=2,colour=colour)`"
              )
      )
    }
    data <- data.frame(xintercept = xintercept)
    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomVline <- ggproto("GeomVline", Geom,
  draw_panel = function(data, panel_params, coord) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomSegment$draw_panel(unique(data), panel_params, coord)
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = "xintercept",

  draw_key = draw_key_vline
)
