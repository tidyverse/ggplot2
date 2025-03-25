#' @export
#' @rdname geom_linerange
#' @param middle.colour,middle.color,middle.linetype,middle.linewidth
#'   Default aesthetics for the middle line. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param box.colour,box.color,box.linetype,box.linewidth
#'   Default aesthetics for the boxes. Set to `NULL` to inherit from the
#'   data's aesthetics.
geom_crossbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          middle.colour     = NULL,
                          middle.color      = NULL,
                          middle.linetype   = NULL,
                          middle.linewidth  = NULL,
                          box.colour        = NULL,
                          box.color         = NULL,
                          box.linetype      = NULL,
                          box.linewidth     = NULL,
                          fatten = deprecated(),
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  middle_gp <- list(
    colour    = middle.color %||% middle.colour,
    linetype  = middle.linetype,
    linewidth = middle.linewidth
  )

  box_gp <- list(
    colour    = box.color %||% box.colour,
    linetype  = box.linetype,
    linewidth = box.linewidth
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrossbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      middle_gp = middle_gp,
      box_gp = box_gp,
      fatten = fatten,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCrossbar <- ggproto("GeomCrossbar", Geom,
  setup_params = function(data, params) {
    if (lifecycle::is_present(params$fatten %||% deprecated())) {
      deprecate_soft0(
        "3.6.0", "geom_crossbar(fatten)",
        "geom_crossbar(middle.linewidth)"
      )
    } else {
      # For backward compatibility reasons
      params$fatten <- 2.5
    }
    GeomErrorbar$setup_params(data, params)
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    GeomErrorbar$setup_data(data, params)
  },

  default_aes = aes(
    colour = from_theme(ink),
    fill = NA,
    linewidth = from_theme(borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA
  ),

  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),

  draw_key = draw_key_crossbar,

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2.5, width = NULL,
                        flipped_aes = FALSE, middle_gp = NULL, box_gp = NULL) {
    data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)

    middle <- transform(data, x = xmin, xend = xmax, yend = y, linewidth = linewidth * fatten, alpha = NA)
    middle <- data_frame0(!!!defaults(compact(middle_gp), middle))

    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
      !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

    if (has_notch) {
      if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
        cli::cli_inform(c(
          "Notch went outside hinges",
          i = "Do you want {.code notch = FALSE}?"
        ))

      notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

      middle$x <- middle$x + notchindent
      middle$xend <- middle$xend - notchindent

      box <- data_frame0(
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
        colour = rep(data$colour,    11),
        linewidth = rep(data$linewidth, 11),
        linetype  = rep(data$linetype,  11),
        fill = rep(data$fill, 11),
        group = rep(seq_len(nrow(data)), 11)
      )
    } else {
      # No notch
      box <- data_frame0(
        x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
        y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
        alpha = rep(data$alpha, 5),
        colour = rep(data$colour,    5),
        linewidth = rep(data$linewidth, 5),
        linetype = rep(data$linetype,  5),
        fill = rep(data$fill, 5),
        group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
      )
    }
    box <- data_frame0(!!!defaults(compact(box_gp), box))
    box <- flip_data(box, flipped_aes)
    middle <- flip_data(middle, flipped_aes)

    ggname("geom_crossbar", gTree(children = gList(
      GeomPolygon$draw_panel(box, panel_params, coord, lineend = lineend, linejoin = linejoin),
      GeomSegment$draw_panel(middle, panel_params, coord, lineend = lineend, linejoin = linejoin)
    )))
  },

  rename_size = TRUE
)
