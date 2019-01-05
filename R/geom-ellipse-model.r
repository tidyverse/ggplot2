#' Experiment to understand geoms
#'
geom_ellipse_model <- 
  function(mapping = NULL, 
           data = NULL,
           stat = "StatEllipseModel", 
           position = "identity",
           ...,
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEllipseModel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
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
GeomEllipseModel <- ggproto("GeomEllipseModel", Geom,

  setup_data = function(data, params) {
    # to be tested with mtcars
    # should produce lines or a rectangle?
    data$xmin <- 15
    data$xmax <- 30
    data$ymin <- 3
    data$ymax <- 4
    data
  },

  draw_group = function(data, panel_params, coord) {

    common <- list(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    ellipse <- new_data_frame(c(
      list(
        xmin = data$xmin,
        xmax = data$xmax,
        ymin = data$ymin,
        ymax = data$ymax,
        alpha = data$alpha  # is this needed?
      ),
      common
    ))

    ggname("geom_ellipse_model", grobTree(
      GeomSegment$draw_panel(ellipse, panel_params, coord)
    ))
  },

  default_aes = aes(colour = "grey20", fill = "white", size = 0.5, alpha = NA, shape = 19, linetype = "solid"),

  #required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax") 
  required_aes = c("xmin", "xmax", "ymin", "ymax") 
)
