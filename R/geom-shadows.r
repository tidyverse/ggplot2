#' Shadows of the data along the axes
#'
#' @eval rd_aesthetics("geom", "shadows")
#' @inheritParams layer
#' @inheritParams geom_line
#' @param geom,stat Use to override the default connection between
#'   `geom_shadows` and `stat_shadows`
#' @param shadows,anchor
#'   Default aesthetics for shadows. Set to `NULL` to inherit from the
#'   aesthetics used for the box.
#' @examples
#' ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
#'     geom_point() +
#'     expand_limits(x = 0, y = 0) +
#'     geom_shadows() 
#'
#' ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
#'     geom_point() +
#'     geom_shadows(anchor = c(35, 1.35)) 
#' @export
geom_shadows <- function(
  mapping = NULL, 
  data = NULL, 
  stat = "shadows", 
  position = "identity", 
  ...,
  shadows = list("x", "y"), 
  anchor = list(x = 0, y = 0),
  na.rm = FALSE,
  show.legend = NA, 
  inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomShadows,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        shadows = shadows,
        anchor = anchor,  
        na.rm = na.rm,
        ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
# should I set this to GeomPath or to Geom? 
GeomShadows <- ggproto("GeomShadows", Geom, 

  # extra_params = c("na.rm"), 

  # required_aes = c("x", "y"),
  
  # unpack parameters: is this needed inside a geom?
  #setup_params = function(data, params) {
  #  params 
  #},

  # set up the data, e.g. remove missing data
  setup_data = function(data, params) { 
    data 
  }, 

  # set up the parameters, e.g. supply warnings for incorrect input
  setup_params = function(data, params) {
    params
  },

  # Not sure if it's (data, panel_scales, coord, ...
  #               or (data, panel_params, coord,...
  draw_group = function(data, panel_params, coord, shadows, anchor) { 
    # draw_group uses stats returned by compute_group, e.g. xmin
    
    # set common aesthetics
    geom_aes <- list(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )
    
    # merge aesthetics with data calculated in setup_data
    geom_stats <- new_data_frame(c(
        list(xmin = data$xmin, 
             xmax = data$xmax,
             ymin = data$ymin, 
             ymax = data$ymax))) 

    print(geom_stats)  # testing

    geom_data <- new_data_frame(c(geom_stats, geom_aes)) 

    print(geom_data)  # testing

    # Turn data into a GeomPath
    geom_grob <- GeomPath$draw_panel(geom_data, panel_params, coord) 

    print(geom_grob)  # testing

    # Pass the GeomPath to grobTree
    ggname("geom_shadows", grobTree(geom_grob)) 

  },

  # set legend box styles
  draw_key = draw_key_path,

  # set default aesthetics 
  default_aes = aes(
    colour = "blue",
    fill = "red",
    size = 0.5,
    linetype = 1,
    alpha = 0.5
  )

)
