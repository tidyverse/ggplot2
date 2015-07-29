#' @export
#' @rdname geom_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
geom_label <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", parse = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ..., nudge_x = 0, nudge_y = 0,
                       label.padding = grid::unit(0.25, "lines"),
                       label.r = grid::unit(0.15, "lines")) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    geom_params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r
    ),
    params = list(...)
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabel <- ggproto("GeomLabel", Geom,
  draw_groups = function(self, data, ...) {
    if (empty(data)) return(zeroGrob())

    grobs <- lapply(1:nrow(data), function(i) {
      self$draw_one(data[i, , drop = FALSE], ...)
    })
    class(grobs) <- "gList"

    ggname("geom_label", grobTree(children = grobs))
  },

  draw_one = function(data, scales, coordinates, ..., parse = FALSE, na.rm = FALSE,
                  label.padding = grid::unit(0.25, "lines"),
                  label.r = grid::unit(0.15, "lines")) {
    data <- remove_missing(data, na.rm, c("x", "y", "label"), name = "geom_label")
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    coords <- coordinates$transform(data, scales)
    if (is.character(coords$vjust)) {
      coords$vjust <- compute_just(coords$vjust, coords$y)
    }
    if (is.character(coords$hjust)) {
      coords$hjust <- compute_just(coords$hjust, coords$x)
    }

    labelGrob(lab,
      x = unit(coords$x, "native"),
      y = unit(coords$y, "native"),
      just = c(coords$hjust, coords$vjust),
      padding = label.padding,
      r = label.r,
      text.gp = gpar(
        col = coords$colour,
        fontsize = coords$size * .pt,
        fontfamily = coords$family,
        fontface = coords$fontface,
        lineheight = coords$lineheight
      ),
      rect.gp = gpar(
        col = coords$colour,
        fill = alpha(coords$fill, coords$alpha)
      )
    )
  },

  required_aes = c("x", "y", "label"),

  default_aes = aes(colour = "black", fill = "white", size = 5, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2),

  draw_key = draw_key_label
)

labelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  stopifnot(length(label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
    name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob")
}

#' @export
makeContent.labelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- textGrob(
    x$label,
    x$x + 2 * (0.5 - hj) * x$padding,
    x$y + 2 * (0.5 - vj) * x$padding,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  r <- roundrectGrob(x$x, x$y, default.units = "native",
    width = grobWidth(t) + 2 * x$padding,
    height = grobHeight(t) + 2 * x$padding,
    just = c(hj, vj),
    r = x$r,
    gp = x$rect.gp,
    name = "box"
  )

  setChildren(x, gList(r, t))
}
