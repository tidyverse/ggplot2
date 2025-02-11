#' @export
#' @rdname geom_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size `r lifecycle::badge("deprecated")` Replaced by the
#'   `linewidth` aesthetic. Size of label border, in mm.
#' @param border.colour,border.color Colour of label border. When `NULL`
#'   (default), the `colour` aesthetic determines the colour of the label border.
#'   `border.color` is an alias for `border.colour`.
#' @param text.colour,text.color Colour of the text. When `NULL` (default), the
#'   `colour` aesthetic determines the colour of the text. `text.color` is an
#'   alias for `text.colour`.
geom_label <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "nudge",
                       ...,
                       parse = FALSE,
                       label.padding = unit(0.25, "lines"),
                       label.r = unit(0.15, "lines"),
                       label.size = deprecated(),
                       border.colour = NULL,
                       border.color = NULL,
                       text.colour = NULL,
                       text.color = NULL,
                       size.unit = "mm",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  extra_args <- list2(...)
  if (lifecycle::is_present(label.size)) {
    deprecate_warn0("3.5.0", "geom_label(label.size)", "geom_label(linewidth)")
    extra_args$linewidth <- extra_args$linewidth %||% label.size
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
      size.unit = size.unit,
      border.colour = border.color %||% border.colour,
      text.colour = text.color %||% text.colour,
      na.rm = na.rm,
      !!!extra_args
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
    colour = from_theme(ink), fill = from_theme(paper),
    family = from_theme(family),
    size = from_theme(fontsize),
    angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, fontface = 1,
    lineheight = 1.2,
    linewidth = from_theme(borderwidth * 0.5),
    linetype  = from_theme(bordertype)
  ),

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        border.colour = NULL,
                        text.colour = NULL,
                        size.unit = "mm") {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    if (!is.margin("margin")) {
      label.padding <- rep(label.padding, length.out = 4)
    }

    size.unit <- resolve_text_unit(size.unit)
    data$text.colour   <- text.colour %||% data$colour
    data$border.colour <- border.colour %||% data$colour
    data$border.colour[data$linewidth == 0] <- NA
    data$fill <- fill_alpha(data$fill, data$alpha)
    data$size <- data$size * size.unit


    grobs <- lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      labelGrob(lab[i],
        x = unit(row$x, "native"),
        y = unit(row$y, "native"),
        just = c(row$hjust, row$vjust),
        padding = label.padding,
        r = label.r,
        angle = row$angle,
        text.gp = gg_par(
          col = row$text.colour,
          fontsize = row$size,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        rect.gp = gg_par(
          col = row$border.colour,
          fill = row$fill,
          lwd = row$linewidth,
          lty = row$linetype
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
                      text.gp = gpar(), rect.gp = gg_par(fill = "white"), vp = NULL) {

  if (length(label) != 1) {
    cli::cli_abort("{.arg label} must be of length 1.")
  }

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  if (!is.null(angle) & is.null(vp)) {
    vp <- viewport(
      angle = angle, x = x, y = y,
      width = unit(0, "cm"), height = unit(0, "cm"),
      gp = gg_par(fontsize = text.gp$fontsize)
    )
    x <- unit(rep(0.5, length(x)), "npc")
    y <- unit(rep(0.5, length(y)), "npc")
  }

  descent <- font_descent(
    text.gp$fontfamily, text.gp$fontface, text.gp$fontsize, text.gp$cex
  )
  # To balance labels, we ensure the top includes at least the descent height
  # and subtract the descent height from the bottom padding
  padding[1] <- unit.pmax(padding[1], descent)
  padding[3] <- unit.pmax(padding[3] - descent, unit(0, "pt"))

  hjust <- resolveHJust(just, NULL)
  vjust <- resolveVJust(just, NULL)

  text <- titleGrob(
    label = label, hjust = hjust, vjust = vjust, x = x,
    y = y + (1 - vjust) * descent,
    margin = padding, margin_x = TRUE, margin_y = TRUE,
    gp = text.gp
  )

  height <- heightDetails(text)
  box <- roundrectGrob(
    x = x, y = y + (0.5 - vjust) * height,
    width  = widthDetails(text),
    height = height,
    just   = c(hjust, 0.5),
    r = r, gp = rect.gp, name = "box"
  )

  gTree(children = gList(box, text), name = name, vp = vp)
}
