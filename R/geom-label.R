#' @export
#' @rdname geom_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
geom_label <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       label.padding = unit(0.25, "lines"),
                       label.r = unit(0.15, "lines"),
                       label.size = 0.25,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
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
    params = list2(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabel <- ggproto("GeomLabel", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", fill = "white", size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
    lineheight = 1.2
  ),

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    if (!inherits(label.padding, "margin")) {
      label.padding <- rep(label.padding, length.out = 4)
    }

    grobs <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      labelGrob(lab[i],
        x = unit(row$x, "native"),
        y = unit(row$y, "native"),
        just = c(row$hjust, row$vjust),
        padding = label.padding,
        r = label.r,
        angle = row$angle,
        text.gp = gpar(
          col = row$colour,
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        rect.gp = gpar(
          col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
          fill = alpha(row$fill, row$alpha),
          lwd = label.size * .pt
        )
      )
    })
    class(grobs) <- "gList"

    ggname("geom_label", grobTree(children = grobs))
  },

  draw_key = draw_key_label
)

labelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      angle = NULL, default.units = "npc", name = NULL,
                      text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  if (length(label) != 1) {
    cli::cli_abort("{.arg label} must be of length 1")
  }

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  if (!is.null(angle) & is.null(vp)) {
    vp <- viewport(
      angle = angle, x = x, y = y,
      width = unit(0, "cm"), height = unit(0, "cm"),
      gp = gpar(fontsize = text.gp$fontsize)
    )
    x <- unit(rep(0.5, length(x)), "npc")
    y <- unit(rep(0.5, length(y)), "npc")
  }

  descent <- font_descent(
    text.gp$fontfamily, text.gp$fontface, text.gp$fontsize, text.gp$cex
  )
  hjust <- resolveHJust(just, NULL)
  vjust <- resolveVJust(just, NULL)

  text <- titleGrob(
    label = label, hjust = hjust, vjust = vjust, x = x, y = y,
    margin = padding, margin_x = TRUE, margin_y = TRUE,
    gp = text.gp
  )

  box <- roundrectGrob(
    x = x, y = y - (1 - vjust) * descent,
    width  = widthDetails(text),
    height = heightDetails(text),
    just   = c(hjust, vjust),
    r = r, gp = rect.gp, name = "box"
  )

  gTree(children = gList(box, text), name = name, vp = vp)
}
