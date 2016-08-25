#' @include facet-.r
NULL

#' Facet specification: a single panel.
#'
#' @inheritParams facet_grid
#' @export
#' @examples
#' # facet_null is the default facetting specification if you
#' # don't override it with facet_grid or facet_wrap
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
facet_null <- function(x.axis = "bottom", y.axis = "left", x.secondary = NULL, x.sec.name = "", y.secondary = NULL, y.sec.name = "", shrink = TRUE) {
  ggproto(NULL, FacetNull,
    shrink = shrink,
    params = list(
      x.axis = match.arg(x.axis, c("bottom", "top")),
      y.axis = match.arg(y.axis, c("left", "right")),
      x.secondary = x.secondary,
      x.sec.name = x.sec.name,
      y.secondary = y.secondary,
      y.sec.name = y.sec.name
    )
  )
}

FacetNull <- ggproto("FacetNull", Facet,
  shrink = TRUE,

  compute_layout = function(data, params) {
    layout_null()
  },
  map_data = function(data, layout, params) {
    # Need the is.waive check for special case where no data, but aesthetics
    # are mapped to vectors
    if (is.waive(data) || empty(data))
      return(cbind(data, PANEL = integer(0)))
    data$PANEL <- 1L
    data
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    range <- ranges[[1]]

    # Figure out aspect ratio
    aspect_ratio <- theme$aspect.ratio %||% coord$aspect(range)
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    axis_h <- coord$render_axis_h(range, theme, position = params$x.axis)
    axis_v <- coord$render_axis_v(range, theme, position = params$y.axis)

    all <- matrix(list(
      axis_v,     panels[[1]],
      zeroGrob(), axis_h
    ), ncol = 2, byrow = TRUE)
    z_matrix <- matrix(c(3, 2, 1, 4), ncol = 2, byrow = TRUE)
    grob_widths <- unit.c(grobWidth(axis_v), unit(1, "null"))
    grob_heights <- unit.c(unit(aspect_ratio, "null"), grobHeight(axis_h))
    grob_names <- c("axis-l", "spacer", "panel", "axis-b")
    grob_clip <- c("off", "off", "on", "off")
    if (params$x.axis == "top") {
      all <- all[c(2,1),]
      z_matrix <- z_matrix[c(2,1),]
      grob_heights <- rev(grob_heights)
      grob_names <- grob_names[c(2,1,4,3)]
      grob_clip <- grob_clip[c(2,1,4,3)]
    }
    if (params$y.axis == "right") {
      all <- all[,c(2,1)]
      z_matrix <- z_matrix[,c(2,1)]
      grob_widths <- rev(grob_widths)
      grob_names <- grob_names[c(3,4,1,2)]
      grob_clip <- grob_clip[c(3,4,1,2)]
    }

    layout <- gtable_matrix("layout", all,
      widths = grob_widths, heights = grob_heights,
      respect = respect, clip = grob_clip,
      z = z_matrix
    )
    layout$layout$name <- grob_names

    if (!is.null(params$x.secondary)) {
      if (!is.function(x.secondary)) stop("x.secondary must be a function", call. = FALSE)
      sec_x_pos <- if (params$x.axis == "bottom") "top" else "bottom"
      sec_x_range <- params$x.secondary(range$x.range)
      sec_x_scale <- scale_x_continuous(limits = sec_x_range, expand = c(0, 0))
      sec_x_scale$train(sec_x_range)
      sec_x_range <- coord$train(list(x = sec_x_scale, y = sec_x_scale))
      sec_x_axis <- coord$render_axis_h(sec_x_range, theme, position = sec_x_pos)
      sec_x_label <- element_render(theme, "axis.title.x", params$x.sec.name, expand_y = TRUE)
      x_axis_height <- grobHeight(sec_x_axis)
      x_label_height <- grobHeight(sec_x_label)
      panel_x_pos <- if (params$y.axis == "left") 2 else 1
      if (sec_x_pos == "top") {
        layout <- gtable_add_rows(layout, unit.c(x_label_height, x_axis_height), pos = 0)
        layout <- gtable_add_grob(layout, sec_x_label, t = 1, l = panel_x_pos, clip = "off", name = "xlab_sec")
        layout <- gtable_add_grob(layout, sec_x_axis, t = 2, l = panel_x_pos, clip = "off", name = "axis-b_sec")
      } else {
        layout <- gtable_add_rows(layout, unit.c(x_axis_height, x_label_height), pos = -1)
        layout <- gtable_add_grob(layout, sec_x_label, t = -1, l = panel_x_pos, clip = "off", name = "xlab_sec")
        layout <- gtable_add_grob(layout, sec_x_axis, t = -2, l = panel_x_pos, clip = "off", name = "axis-b_sec")
      }
    }
    if (!is.null(params$y.secondary)) {
      if (!is.function(y.secondary)) stop("y.secondary must be a function", call. = FALSE)
      sec_y_pos <- if (params$y.axis == "left") "right" else "left"
      sec_y_range <- params$y.secondary(range$y.range)
      sec_y_scale <- scale_y_continuous(limits = sec_y_range, expand = c(0, 0))
      sec_y_scale$train(sec_y_range)
      sec_y_range <- coord$train(list(x = sec_y_scale, y = sec_y_scale))
      sec_y_axis <- coord$render_axis_v(sec_y_range, theme, position = sec_y_pos)
      sec_y_label <- element_render(theme, "axis.title.y", params$y.sec.name, expand_x = TRUE)
      y_axis_width <- grobWidth(sec_y_axis)
      y_label_width <- grobWidth(sec_y_label)
      panel_y_pos <- if (params$x.axis == "top") 2 else 1
      if (sec_y_pos == "left") {
        layout <- gtable_add_cols(layout, unit.c(y_label_width, y_axis_width), pos = 0)
        layout <- gtable_add_grob(layout, sec_y_label, t = panel_y_pos, l = 1, clip = "off", name = "ylab_sec")
        layout <- gtable_add_grob(layout, sec_y_axis, t = panel_y_pos, l = 2, clip = "off", name = "axis-l_sec")
      } else {
        layout <- gtable_add_cols(layout, unit.c(y_axis_width, y_label_width), pos = -1)
        layout <- gtable_add_grob(layout, sec_y_label, t = panel_y_pos, l = -1, clip = "off", name = "xlab_sec")
        layout <- gtable_add_grob(layout, sec_y_axis, t = panel_y_pos, l = -2, clip = "off", name = "axis-l_sec")
      }
    }
    layout
  },
  vars = function(self) {
    ""
  }
)
