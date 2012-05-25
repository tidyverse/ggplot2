#' A theme with grey background and white gridlines.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @keywords dplot
#' @aliases theme_gray theme_grey
#' @export theme_gray theme_grey
theme_grey <- function(base_size = 12, base_family = "") {
  structure(list(
    line               = element_line(colour = "black", size = 0.5, linetype = 1),
    rect               = element_rect(fill = NA, colour = "black", size = 0.5, linetype = 1),
    segment            = element_segment(colour = "black", size = 0.5, linetype = 1),
    text               = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text          = element_text(size = rel(0.8), colour = "grey50"),

    axis.line =          element_blank(),
    axis.text.x =        element_text(vjust = 1),
    axis.text.y =        element_text(hjust = 1),
    axis.ticks =         element_segment(colour = "grey50"),
    axis.title.x =       element_text(),
    axis.title.y =       element_text(angle = 90),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour="white"),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "grey95", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "grey90", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "white"),
    panel.grid.minor =   element_line(colour = "grey95", size = 0.25),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = "grey80", colour = NA),
    strip.text.x =       element_text(size = rel(0.8)),
    strip.text.y =       element_text(size = rel(0.8), angle = -90),

    plot.background =    element_rect(colour = NA, fill = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}
theme_gray <- theme_grey

#' A theme with white background and black gridlines.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @keywords dplot
#' @export
theme_bw <- function(base_size = 12, base_family = "") {
  structure(list(
    text = element_text( size = base_size),
    axis.line =          element_blank(),
    axis.text.x =        element_text(family = base_family, size = rel(0.5), lineheight = 0.9, vjust = 1),
    axis.text.y =        element_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =         element_segment(colour = "black", size = 0.2),
    axis.title.x =       element_text(family = base_family, size = base_size, vjust = 0.5),
    axis.title.y =       element_text(family = base_family, size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  element_rect(colour=NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(colour = "grey80"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(family = base_family, size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       element_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "grey50"),
    panel.grid.major =   element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =   element_line(colour = "grey98", size = 0.5),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   element_rect(fill = "grey80", colour = "grey50"),
    strip.text.x =       element_text(family = base_family, size = base_size * 0.8),
    strip.text.y =       element_text(family = base_family, size = base_size * 0.8, angle = -90),

    plot.background =    element_rect(colour = NA),
    plot.title =         element_text(family = base_family, size = base_size * 1.2),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

