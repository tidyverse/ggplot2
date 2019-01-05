#' Shadows of the data along the axes
#'
#' @eval rd_aesthetics("geom", "shadow")
#' @inheritParams layer
#' @param type, anchor, quantile, variable
#'   Default aesthetics for shadow. Set to `NULL` to inherit from the
#'   aesthetics used for the stats.
#' @examples
#' ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
#'     geom_point() +
#'     expand_limits(x = 0, y = 0) +
#'     geom_shadow() 
#'
#' ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
#'     geom_point() +
#'     geom_shadow(anchor = c(35, 1.35)) 
#' @export
geom_shadow <- function(
  mapping = NULL, 
  data = NULL, 
  stat = "shadow", 
  position = "identity", 
  ...,
  type = NULL,
  anchor = list(x = 0, y = 0),
  quantile = NULL,
  variable = list("x", "y"), 
  na.rm = FALSE,
  show.legend = NA, 
  inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomShadow,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        type = type,
        anchor = anchor,  
        quantile = quantile,
        variable = variable,
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
GeomShadow <- ggproto("GeomShadow", Geom, 

  # required_aes are set in stat_shadow 
  
  # extra_params 
  # Most parameters are inherited from draw_panel() or draw_groups(). 
  # Parameters needed in setup_data() or handle_na() can be passed to extra_params.
  extra_params = c("na.rm", "type", "anchor", "quantile", "variable"),

  # set up the data, e.g. remove missing data
    # data inherited from stat_shadow. Example of format:
    # PANEL  ymin  ymax  x  group  xmin  xmax  
    #     1  1.51  4.07  6     -1   4.2   7.8
  setup_data = function(data, params) { 
    data 
  }, 

  # set up the parameters, e.g. supply warnings for incorrect input
  setup_params = function(data, params) {
    params
  },

  # set up the grobs for drawing 
    # data format example (boxplot whiskers):
    #    x xend  y  yend  alpha  colour size linetype    fill group
    # 1 10  33   0     0      1   black    5        2  grey20    -1
    # 2  0   0   2     5      1   black    5        2  grey20    -1
  draw_group = function(data, panel_params, coord) { 
    
    # draw_group uses stats returned by compute_group

    if (!coord$is_linear()) {
      warning("geom_shadow is not implemented for non-linear coordinates",
        call. = FALSE)
    }
    
    # set common aesthetics
    geom_aes <- list(
      alpha = data$alpha,
      colour = data$color %||% data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    # set up data computed in stat
    geom_stat <- list(
      x = c(data$x.xmin, data$y.xmin),
      xend = c(data$x.xmax, data$y.xmax),
      y = c(data$x.ymin, data$y.ymin),
      yend = c(data$x.ymax, data$y.ymax)
    )

    # merge aesthetics with data calculated in setup_data
    geom_data <- new_data_frame(c(geom_stat, geom_aes), n = 2) 
    
    # turn the stats data into a GeomSegment
    geom_grob <- GeomSegment$draw_panel(unique(geom_data), panel_params, coord) 

    # pass the GeomSegment to grobTree
    ggname("geom_shadow", grobTree(geom_grob)) 

  },

  # set legend box styles
  #draw_key = draw_key_shadow, # after adding lineends
  draw_key = draw_key_path,

  # set default aesthetics 
  default_aes = aes(
    colour = "black", 
    fill = "grey20",
    size = 1,
    linetype = 1,
    alpha = 1
  )

)

# end of geom-shadow.r