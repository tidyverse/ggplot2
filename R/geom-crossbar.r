#' @export
#' @rdname geom_linerange
geom_crossbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          fatten = 2.5,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrossbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCrossbar <- ggproto("GeomCrossbar", Geom,
  setup_data = function(data, params) {
    GeomErrorbar$setup_data(data, params)
  },

  default_aes = aes(x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL,
    ymax = NULL, colour = "black", fill = NA, size = 0.5, linetype = 1,
    alpha = NA),

  draw_key = draw_key_crossbar,

  draw_panel = function(data, panel_params, coord, fatten = 2.5, width = NULL) {
    main_aes <- data$main_aes[1] %||% "x"
    if (main_aes == "y") names(data) <- switch_position(names(data))
    middle <- transform(data, x = xmin, xend = xmax, yend = y, size = size * fatten, alpha = NA)

    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
      !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

    if (has_notch) {
      if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
        message("notch went outside hinges. Try setting notch=FALSE.")

      notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

      middle$x <- middle$x + notchindent
      middle$xend <- middle$xend - notchindent

      box <- new_data_frame(list(
        x = c(
          data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
          data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
          data$xmin
        ),
        y = c(
          data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
          data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
          data$ymax
        ),
        alpha = rep(data$alpha, 11),
        colour = rep(data$colour, 11),
        size = rep(data$size, 11),
        linetype = rep(data$linetype, 11),
        fill = rep(data$fill, 11),
        group = rep(seq_len(nrow(data)), 11)
      ))
    } else {
      # No notch
      box <- new_data_frame(list(
        x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
        y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
        alpha = rep(data$alpha, 5),
        colour = rep(data$colour, 5),
        size = rep(data$size, 5),
        linetype = rep(data$linetype, 5),
        fill = rep(data$fill, 5),
        group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
      ))
    }
    if (main_aes == "y") {
      names(box) <- switch_position(names(box))
      names(middle) <- switch_position(names(middle))
    }
    ggname("geom_crossbar", gTree(children = gList(
      GeomPolygon$draw_panel(box, panel_params, coord),
      GeomSegment$draw_panel(middle, panel_params, coord)
    )))
  }
)
