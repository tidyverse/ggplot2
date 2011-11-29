#' A theme with grey background and white gridlines.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @keywords dplot
#' @aliases theme_gray theme_grey
#' @export theme_gray theme_grey
theme_grey <- function(base_size = 12, base_family = "") {
  structure(list(
    axis.line =          theme_blank(),
    axis.text.x =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, colour = "grey50", vjust = 1),
    axis.text.y =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
    axis.ticks =         theme_segment(colour = "grey50"),
    axis.title.x =       theme_text(family = base_family, size = base_size, vjust = 0.5),
    axis.title.y =       theme_text(family = base_family, size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  theme_rect(colour="white"),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         theme_rect(fill = "grey95", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        theme_text(family = base_family, size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       theme_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   theme_rect(fill = "grey90", colour = NA),
    panel.border =       theme_blank(),
    panel.grid.major =   theme_line(colour = "white"),
    panel.grid.minor =   theme_line(colour = "grey95", size = 0.25),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   theme_rect(fill = "grey80", colour = NA),
    strip.text.x =       theme_text(family = base_family, size = base_size * 0.8),
    strip.text.y =       theme_text(family = base_family, size = base_size * 0.8, angle = -90),

    plot.background =    theme_rect(colour = NA, fill = "white"),
    plot.title =         theme_text(family = base_family, size = base_size * 1.2),
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
    axis.line =          theme_blank(),
    axis.text.x =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, vjust = 1),
    axis.text.y =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =         theme_segment(colour = "black", size = 0.2),
    axis.title.x =       theme_text(family = base_family, size = base_size, vjust = 0.5),
    axis.title.y =       theme_text(family = base_family, size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),

    legend.background =  theme_rect(colour=NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         theme_rect(colour = "grey80"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        theme_text(family = base_family, size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       theme_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   theme_rect(fill = "white", colour = NA),
    panel.border =       theme_rect(fill = NA, colour = "grey50"),
    panel.grid.major =   theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =   theme_line(colour = "grey98", size = 0.5),
    panel.margin =       unit(0.25, "lines"),

    strip.background =   theme_rect(fill = "grey80", colour = "grey50"),
    strip.text.x =       theme_text(family = base_family, size = base_size * 0.8),
    strip.text.y =       theme_text(family = base_family, size = base_size * 0.8, angle = -90),

    plot.background =    theme_rect(colour = NA),
    plot.title =         theme_text(family = base_family, size = base_size * 1.2),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

