#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These are the options built into ggplot2.
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @keywords internal
#' @name draw_key
NULL

#' @export
#' @rdname draw_key
draw_key_point <- function(data, params, size) {
  pointsGrob(0.5, 0.5,
    pch = data$shape,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fill = alpha(data$fill, data$alpha),
      fontsize = data$size * .pt + data$stroke * .stroke / 2,
      lwd = data$stroke * .stroke / 2
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_abline <- function(data, params, size) {
  segmentsGrob(0, 0, 1, 1,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_rect <- function(data, params, size) {
  rectGrob(gp = gpar(
    col = NA,
    fill = alpha(data$fill, data$alpha),
    lty = data$linetype
  ))
}
#' @export
#' @rdname draw_key
draw_key_polygon <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  rectGrob(
    width = unit(1, "npc") - unit(lwd, "mm"),
    height = unit(1, "npc") - unit(lwd, "mm"),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
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
  grobTree(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_crossbar <- function(data, params, size) {
  grobTree(
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_path <- function(data, params, size) {
  segmentsGrob(0.1, 0.5, 0.9, 0.5,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    ),
    arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key
draw_key_vpath <- function(data, params, size) {
  segmentsGrob(0.5, 0.1, 0.5, 0.9,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    ),
    arrow = params$arrow
  )
}

#' @export
#' @rdname draw_key
draw_key_dotplot <- function(data, params, size) {
  pointsGrob(0.5, 0.5, size = unit(.5, "npc"),
    pch = 21,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fill = alpha(data$fill, data$alpha)
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_pointrange <- function(data, params, size) {
  grobTree(
    draw_key_vpath(data, params, size),
    draw_key_point(transform(data, size = data$size * 4), params)
  )
}

#' @export
#' @rdname draw_key
draw_key_smooth <- function(data, params, size) {
  data$fill <- alpha(data$fill, data$alpha)
  data$alpha <- 1

  grobTree(
    if (isTRUE(params$se)) rectGrob(gp = gpar(col = NA, fill = data$fill)),
    draw_key_path(data, params)
  )
}

#' @export
#' @rdname draw_key
draw_key_text <- function(data, params, size) {
  textGrob("a", 0.5, 0.5,
    rot = data$angle,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fontfamily = data$family,
      fontface = data$fontface,
      fontsize = data$size * .pt
    )
  )
}

#' @export
#' @rdname draw_key
draw_key_label <- function(data, params, size) {
  grobTree(
    draw_key_rect(data, list()),
    draw_key_text(data, list())
  )
}

#' @export
#' @rdname draw_key
draw_key_vline <- function(data, params, size) {
  segmentsGrob(0.5, 0, 0.5, 1,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}
