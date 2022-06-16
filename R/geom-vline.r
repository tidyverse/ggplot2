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
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg mapping} because {.arg xintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg data} because {.arg xintercept} was provided.")
    }

    data <- new_data_frame(list(xintercept = xintercept))
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
    params = list2(
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
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomSegment$draw_panel(unique(data), panel_params, coord, lineend = lineend)
  },

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  required_aes = "xintercept",

  draw_key = draw_key_vline,

  rename_size = TRUE
)
