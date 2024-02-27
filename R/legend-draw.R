#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below.)
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @examples
#' p <- ggplot(economics, aes(date, psavert, color = "savings rate"))
#' # key glyphs can be specified by their name
#' p + geom_line(key_glyph = "timeseries")
#'
#' # key glyphs can be specified via their drawing function
#' p + geom_line(key_glyph = draw_key_rect)
#' @name draw_key
NULL

#' @export
#' @rdname draw_key
draw_key_point <- function(data, params, size) {
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }

  # NULL means the default stroke size, and NA means no stroke.
  stroke_size <- data$stroke %||% 0.5
  stroke_size[is.na(stroke_size)] <- 0

  pointsGrob(0.5, 0.5,
    pch = data$shape,
    gp = gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = fill_alpha(data$fill %||% "black", data$alpha),
      fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke / 2,
      lwd = stroke_size * .stroke / 2
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_abline <- function(data, params, size) {
  segmentsGrob(0, 0, 1, 1,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_rect <- function(data, params, size) {
  rectGrob(gp = gpar(
    col = NA,
    fill = fill_alpha(data$fill %||% data$colour %||% "grey20", data$alpha),
    lty = data$linetype %||% 1
  ))
}
#' @export
#' @rdname draw_key
draw_key_polygon <- function(data, params, size) {
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }

  lwd <- min(data$linewidth, min(size) / 4)

  rectGrob(
    width = unit(1, "npc") - unit(lwd, "mm"),
    height = unit(1, "npc") - unit(lwd, "mm"),
    gp = gpar(
      col = data$colour %||% NA,
      fill = fill_alpha(data$fill %||% "grey20", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
  ))
}

#' @export
#' @rdname draw_key
draw_key_blank <- function(data, params, size) {
  zeroGrob()
}

#' @export
#' @rdname draw_key
draw_key_boxplot <- function(data, params, size) {
  gp <- gpar(
    col = data$colour %||% "grey20",
    fill = fill_alpha(data$fill %||% "white", data$alpha),
    lwd = (data$linewidth %||% 0.5) * .pt,
    lty = data$linetype %||% 1,
    lineend = params$lineend %||% "butt",
    linejoin = params$linejoin %||% "mitre"
  )

  if (isTRUE(params$flipped_aes)) {
    grobTree(
      linesGrob(c(0.1, 0.25), 0.5),
      linesGrob(c(0.75, 0.9), 0.5),
      rectGrob(width = 0.5, height = 0.75),
      linesGrob(0.5, c(0.125, 0.875)),
      gp = gp
    )
  } else {
    grobTree(
      linesGrob(0.5, c(0.1, 0.25)),
      linesGrob(0.5, c(0.75, 0.9)),
      rectGrob(height = 0.5, width = 0.75),
      linesGrob(c(0.125, 0.875), 0.5),
      gp = gp
    )
  }
}

#' @export
#' @rdname draw_key
draw_key_crossbar <- function(data, params, size) {
  gp <- gpar(
    col = data$colour %||% "grey20",
    fill = fill_alpha(data$fill %||% "white", data$alpha),
    lwd = (data$linewidth %||% 0.5) * .pt,
    lty = data$linetype %||% 1,
    lineend = params$lineend %||% "butt",
    linejoin = params$linejoin %||% "mitre"
  )
  if (isTRUE(params$flipped_aes)) {
    grobTree(
      rectGrob(height = 0.75, width = 0.5),
      linesGrob(0.5, c(0.125, 0.875)),
      gp = gp
    )
  } else {
    grobTree(
      rectGrob(height = 0.5, width = 0.75),
      linesGrob(c(0.125, 0.875), 0.5),
      gp = gp
    )
  }
}

#' @export
#' @rdname draw_key
draw_key_path <- function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  grob <- segmentsGrob(0.1, 0.5, 0.9, 0.5,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      fill = alpha(params$arrow.fill %||% data$colour
                   %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = params$arrow
  )
  if (!is.null(params$arrow)) {
    angle <- deg2rad(params$arrow$angle)
    length <- convertUnit(params$arrow$length, "cm", valueOnly = TRUE)
    attr(grob, "width")  <- cos(angle) * length * 1.25
    attr(grob, "height") <- sin(angle) * length * 2
  }
  grob
}

#' @export
#' @rdname draw_key
draw_key_vpath <- function(data, params, size) {
  grob <- segmentsGrob(0.5, 0.1, 0.5, 0.9,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = params$arrow
  )
  if (!is.null(params$arrow)) {
    angle <- deg2rad(params$arrow$angle)
    length <- convertUnit(params$arrow$length, "cm", valueOnly = TRUE)
    attr(grob, "width")  <- sin(angle) * length * 2
    attr(grob, "height") <- cos(angle) * length * 1.25
  }
  grob
}

#' @export
#' @rdname draw_key
draw_key_dotplot <- function(data, params, size) {
  pointsGrob(0.5, 0.5, size = unit(.5, "npc"),
    pch = 21,
    gp = gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = fill_alpha(data$fill %||% "black", data$alpha),
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_linerange <- function(data, params, size) {
  if (isTRUE(params$flipped_aes)) {
    draw_key_path(data, params, size)
  } else {
    draw_key_vpath(data, params, size)
  }
}

#' @export
#' @rdname draw_key
draw_key_pointrange <- function(data, params, size) {
  linerange <- draw_key_linerange(data, params, size)
  grob <- grobTree(
    linerange,
    draw_key_point(transform(data, size = (data$size %||% 1.5) * 4), params)
  )
  attr(grob, "width")  <- attr(linerange, "width")
  attr(grob, "height") <- attr(linerange, "height")
  grob
}

#' @export
#' @rdname draw_key
draw_key_smooth <- function(data, params, size) {
  data$fill <- alpha(data$fill %||% "grey60", data$alpha)
  data$alpha <- 1

  path <- draw_key_path(data, params, size)

  grob <- grobTree(
    if (isTRUE(params$se)) rectGrob(gp = gpar(col = NA, fill = data$fill)),
    path
  )
  attr(grob, "width") <- attr(path, "width")
  attr(grob, "height") <- attr(path, "height")
  grob
}

#' @export
#' @rdname draw_key
draw_key_text <- function(data, params, size) {
  data  <- replace_null(unclass(data), label = "a", angle = 0)
  hjust <- compute_just(data$hjust %||% 0.5)
  vjust <- compute_just(data$vjust %||% 0.5)
  just  <- rotate_just(data$angle, hjust, vjust)
  grob  <- titleGrob(
    data$label,
    x = unit(just$hjust, "npc"), y = unit(just$vjust, "npc"),
    angle = data$angle,
    hjust = hjust,
    vjust = vjust,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      fontfamily = data$family   %||% "",
      fontface   = data$fontface %||% 1,
      fontsize   = (data$size %||% 3.88) * .pt
    ),
    margin = margin(0.1, 0.1, 0.1, 0.1, unit = "lines"),
    margin_x = TRUE, margin_y = TRUE
  )
  attr(grob, "width")  <- convertWidth(grobWidth(grob),   "cm", valueOnly = TRUE)
  attr(grob, "height") <- convertHeight(grobHeight(grob), "cm", valueOnly = TRUE)
  grob
}

#' @export
#' @rdname draw_key
draw_key_label <- function(data, params, size) {
  data <- replace_null(unclass(data), label = "a", angle = 0)
  params$label.size <- params$label.size %||% 0.25
  hjust <- compute_just(data$hjust %||% 0.5)
  vjust <- compute_just(data$vjust %||% 0.5)
  just  <- rotate_just(data$angle, hjust, vjust)
  padding <- rep(params$label.padding %||% unit(0.25, "lines"), length.out = 4)
  descent <- font_descent(
    family = data$family %||% "",
    face = data$fontface %||% 1,
    size = data$size %||% 3.88
  )
  grob <- labelGrob(
    data$label,
    x = unit(just$hjust, "npc"),
    y = unit(just$vjust, "npc") + descent,
    angle = data$angle,
    just = c(hjust, vjust),
    padding = padding,
    r = params$label.r %||% unit(0.15, "lines"),
    text.gp = gpar(
      col = data$colour %||% "black",
      fontfamily = data$family   %||% "",
      fontface   = data$fontface %||% 1,
      fontsize   = (data$size %||% 3.88) * .pt
    ),
    rect.gp = gpar(
      col  = if (isTRUE(all.equal(params$label.size, 0))) NA else data$colour,
      fill = alpha(data$fill %||% "white", data$alpha),
      lwd  = params$label.size * .pt
    )
  )
  angle  <- deg2rad(data$angle %||% 0)
  text   <- grob$children[[2]]
  width  <- convertWidth(grobWidth(text),   "cm", valueOnly = TRUE)
  height <- convertHeight(grobHeight(text), "cm", valueOnly = TRUE)
  x <- c(0, 0, width, width)
  y <- c(0, height, height, 0)
  attr(grob, "width")  <- diff(range(x * cos(angle) - y * sin(angle)))
  attr(grob, "height") <- diff(range(x * sin(angle) + y * cos(angle)))
  grob
}

#' @export
#' @rdname draw_key
draw_key_vline <- function(data, params, size) {
  segmentsGrob(0.5, 0, 0.5, 1,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_timeseries <- function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }

  grid::linesGrob(
    x = c(0, 0.4, 0.6, 1),
    y = c(0.1, 0.6, 0.4, 0.9),
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt",
      linejoin = params$linejoin %||% "round"
    )
  )
}
