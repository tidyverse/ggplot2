#' @include stat-.R
NULL

#' @export
#' @rdname geom_abline
geom_hline <- function(mapping = NULL, data = NULL,
                       ...,
                       yintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(yintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg mapping} because {.arg yintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg data} because {.arg yintercept} was provided.")
    }

    data <- data_frame0(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomHline,
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
GeomHline <- ggproto("GeomHline", Geom,
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomSegment$draw_panel(unique0(data), panel_params, coord, lineend = lineend)
  },

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  required_aes = "yintercept",

  draw_key = draw_key_path,

  rename_size = TRUE,

  check_constant_aes = FALSE
)
