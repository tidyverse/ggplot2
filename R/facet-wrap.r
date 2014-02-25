#' Wrap a 1d ribbon of panels into 2d.
#'
#' @param nrow number of rows
#' @param ncol number of columns
#' @param facets formula specifying variables to facet by
#' @param scales should scales be fixed (\code{"fixed"}, the default),
#'   free (\code{"free"}), or free in one dimension  (\code{"free_x"},
#'   \code{"free_y"})
#' @inheritParams facet_grid
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
#'   xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
#' d + facet_wrap(~ color)
#' d + facet_wrap(~ color, ncol = 1)
#' d + facet_wrap(~ color, ncol = 4)
#' d + facet_wrap(~ color, nrow = 1)
#' d + facet_wrap(~ color, nrow = 3)
#'
#' # Using multiple variables continues to wrap the long ribbon of
#' # plots into 2d - the ribbon just gets longer
#' # d + facet_wrap(~ color + cut)
#'
#' # To change plot order of facet wrap,
#' # change the order of varible levels with factor()
#' diamonds$color <- factor(diamonds$color, levels = c("G", "J", "D", "E", "I", "F", "H"))
#' # Repeat first example with new order
#' d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
#' xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
#' d + facet_wrap(~ color)
#'
#' # You can choose to keep the scales constant across all panels
#' # or vary the x scale, the y scale or both:
#' p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
#' p + facet_wrap(~ color)
#' p + facet_wrap(~ color, scales = "free_y")
#'
#' p <- qplot(displ, hwy, data = mpg)
#' p + facet_wrap(~ cyl)
#' p + facet_wrap(~ cyl, scales = "free")
#'
#' # Use as.table to to control direction of horizontal facets, TRUE by default
#' p + facet_wrap(~ cyl, as.table = FALSE)
#'
#' # Add data that does not contain all levels of the faceting variables
#' cyl6 <- subset(mpg, cyl == 6)
#' p + geom_point(data = cyl6, colour = "red", size = 1) +
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") +
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = NULL), colour = "red") +
#'   facet_wrap(~ cyl)
#' }
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, as.table = TRUE, drop = TRUE) {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  facet(
    facets = as.quoted(facets), free = free, shrink = shrink,
    as.table = as.table, drop = drop,
    ncol = ncol, nrow = nrow,
    subclass = "wrap"
  )
}

#' @export
facet_train_layout.wrap <- function(facet, data) {
  panels <- layout_wrap(data, facet$facets, facet$nrow, facet$ncol,
     facet$as.table, facet$drop)

  n <- nrow(panels)
  nrow <- max(panels$ROW)

  # Add scale identification
  panels$SCALE_X <- if (facet$free$x) seq_len(n) else 1L
  panels$SCALE_Y <- if (facet$free$y) seq_len(n) else 1L

  # Figure out where axes should go
  panels$AXIS_X <- if (facet$free$x) TRUE else panels$ROW == nrow
  panels$AXIS_Y <- if (facet$free$y) TRUE else panels$COL == 1

  panels
}

#' @export
facet_map_layout.wrap <- function(facet, data, layout) {
  locate_wrap(data, layout, facet$facets)
}

# How to think about facet wrap:
#  * vector of panels
#  * every panel has strips (strip_pos) and axes (axis_pos)
#  * if scales fixed, most axes empty
#  * combine panels, strips and axes, then wrap into 2d
#  * finally: add title, labels and legend
#
#' @export
facet_render.wrap <- function(facet, panel, coord, theme, geom_grobs) {

  # If coord is (non-cartesian or flip) and (x is free or y is free)
  # then print a warning
  if ((!inherits(coord, "cartesian") || inherits(coord, "flip")) &&
    (facet$free$x || facet$free$y)) {
    stop("ggplot2 does not currently support free scales with a non-cartesian coord or coord_flip.\n")
  }

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !facet$free$x && !facet$free$y) {
    aspect_ratio <- coord_aspect(coord, panel$ranges[[1]])
  }

  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  layout <- panel$layout
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)

  panels <- facet_panels(facet, panel, coord, theme, geom_grobs)
  axes <- facet_axes(facet, panel, coord, theme)
  strips <- facet_strips(facet, panel, theme)

  # Should become facet_arrange_grobs

  # Locate each element in panel
  find_pos <- function(layout, loc, size) {
    n <- nrow(layout)
    l <- size[1] * (layout$COL - 1) + loc[1]
    t <- size[2] * (layout$ROW - 1) + loc[2]
    data.frame(t = t, r = l, b = t, l = l, id = seq_len(n))
  }
  locs <- list(
    panel =   c(2, 2),
    strip_t = c(2, 1),
    axis_l =  c(1, 2),
    axis_b =  c(2, 3),
    hspace =  c(2, 4),
    vspace =  c(3, 2)
  )
  grobs <- list(
    panel = panels,
    strip_t = strips$t,
    axis_l = axes$l,
    axis_b = axes$b
  )

  info <- ldply(locs, find_pos, layout = layout, size = c(3, 4))
  names(info)[1] <- "type"
  info$clip <- ifelse(info$type == "panel", "on", "off")
  info$name <- paste(info$type, info$id, sep = "-")

  # Bare numbers are taken as cm
  # If not listed, assume is unit(1, "null")
  widths <- list(
    axis_l = width_cm(grobs$axis_l),
    vspace = ifelse(layout$COL == ncol, 0, width_cm(theme$panel.margin.x %||% theme$panel.margin))
  )
  heights <- list(
    panel = unit(aspect_ratio, "null"),
    strip_t = height_cm(grobs$strip_t),
    axis_b = height_cm(grobs$axis_b),
    hspace = ifelse(layout$ROW == nrow, 0, height_cm(theme$panel.margin.y %||% theme$panel.margin))
  )

  col_widths <- compute_grob_widths(info, widths)
  row_heights <- compute_grob_heights(info, heights)

  # Create the gtable for the legend
  gt <- gtable(widths = col_widths, heights = row_heights, respect = respect)

  # Keep only the rows in info that refer to grobs
  info  <- info[info$type %in% names(grobs), ]
  grobs <- unlist(grobs, recursive = FALSE)
  # Add the grobs
  gt <- gtable_add_grob(gt, grobs, l = info$l, t = info$t, r = info$r,
    b = info$b, name = info$name, clip = info$clip)

  gt
}

#' @export
facet_panels.wrap <- function(facet, panel, coord, theme, geom_grobs) {
  panels <- panel$layout$PANEL
  lapply(panels, function(i) {
    fg <- coord_render_fg(coord, panel$range[[i]], theme)
    bg <- coord_render_bg(coord, panel$range[[i]], theme)

    geom_grobs <- lapply(geom_grobs, "[[", i)
    panel_grobs <- c(list(bg), geom_grobs, list(fg))

    ggname(paste("panel", i, sep = "-"),
      gTree(children = do.call("gList", panel_grobs)))
  })
}

#' @export
facet_strips.wrap <- function(facet, panel, theme) {
  labels_df <- panel$layout[names(facet$facets)]
  labels_df[] <- llply(labels_df, format, justify = "none")

  labels <- apply(labels_df, 1, paste, collapse=", ")

  list(t = llply(labels, ggstrip, theme = theme))
}

#' @export
facet_axes.wrap <- function(facet, panel, coord, theme) {
  panels <- panel$layout$PANEL

  axes <- list()
  axes$b <- lapply(panels, function(i) {
    if (panel$layout$AXIS_X[i]) {
      grob <- coord_render_axis_h(coord, panel$range[[i]], theme)
    } else {
      grob <- zeroGrob()
    }
    ggname(paste("axis-b-", i, sep = ""), grob)
  })

  axes$l <- lapply(panels, function(i) {
    if (panel$layout$AXIS_Y[i]) {
      grob <- coord_render_axis_v(coord, panel$range[[i]], theme)
    } else {
      grob <- zeroGrob()
    }
    ggname(paste("axis-l-", i, sep = ""), grob)
  })
  axes

}

#' @export
facet_vars.wrap <- function(facet) {
  paste(lapply(facet$facets, paste, collapse = ", "), collapse = " ~ ")
}
