#' ggplot2 themes
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @details \describe{
#'
#' \item{\code{theme_gray}}{
#' The signature ggplot2 theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.}
#'
#' \item{\code{theme_bw}}{
#' The classic dark-on-light ggplot2 theme. May work better for presentations
#' displayed with a projector.}
#'
#' \item{\code{theme_linedraw}}{
#' A theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawings. Serves a purpose similar to \code{theme_bw}.
#' Note that this theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{\code{theme_light}}{
#' A theme similar to \code{theme_linedraw} but with light grey lines and axes,
#' to direct more attention towards the data.}
#'
#' \item{\code{theme_dark}}{
#' The dark cousin of \code{theme_light}, with similar line sizes but a dark background. Useful to make thin coloured lines pop out.}
#'
#' \item{\code{theme_minimal}}{
#' A minimalistic theme with no background annotations.}
#'
#' \item{\code{theme_classic}}{
#' A classic-looking theme, with x and y axis lines and no gridlines.}
#'
#' \item{\code{theme_void}}{
#' A completely empty theme.}
#'
#' }
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_gray()
#' p + theme_bw()
#' p + theme_linedraw()
#' p + theme_light()
#' p + theme_dark()
#' p + theme_minimal()
#' p + theme_classic()
#' p + theme_void()
#'
#' @name ggtheme
NULL

#' @export
#' @rdname ggtheme
theme_grey <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black",
                            size = 0.5, linetype = 1),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    axis.line =          element_line(),
    axis.line.x =        element_blank(),
    axis.line.y =        element_blank(),
    axis.text =          element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =         element_line(colour = "grey20"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
                           margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
                         ),

    legend.background =  element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = "grey95", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = "grey92", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "white"),
    panel.grid.minor =   element_line(colour = "white", size = 0.25),
    panel.margin =       unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(
                           size = rel(1.2),
                           margin = margin(b = half_line * 1.2)
                         ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
#' @export
#' @rdname ggtheme
theme_gray <- theme_grey

#' @export
#' @rdname ggtheme
theme_bw <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = element_text(size = rel(0.8)),
      axis.ticks        = element_line(colour = "black"),
      legend.key        = element_rect(colour = "grey80"),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      strip.background  = element_rect(fill = "grey80", colour = "grey50", size = 0.2)
    )
}

#' @export
#' @rdname ggtheme
theme_linedraw <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = element_text(colour = "black", size = rel(0.8)),
      axis.ticks        = element_line(colour = "black", size = 0.25),
      legend.key        = element_rect(colour = "black", size = 0.25),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "black", size = 0.5),
      panel.grid.major  = element_line(colour = "black", size = 0.05),
      panel.grid.minor  = element_line(colour = "black", size = 0.01),
      strip.background  = element_rect(fill = "black", colour = NA),
      strip.text.x      = element_text(
                            colour = "white",
                            margin = margin(t = half_line, b = half_line)
                          ),
      strip.text.y      = element_text(
                            colour = "white",
                            angle = 90,
                            margin = margin(l = half_line, r = half_line)
                          )
    )
}

#' @export
#' @rdname ggtheme
theme_light <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.ticks        = element_line(colour = "grey70", size = 0.25),
      legend.key        = element_rect(fill = "white", colour = "grey50", size = 0.25),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey70", size = 0.5),
      panel.grid.major  = element_line(colour = "grey85", size = 0.25),
      panel.grid.minor  = element_line(colour = "grey93", size = 0.125),
      strip.background  = element_rect(fill = "grey70", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = -90,
        margin = margin(l = half_line, r = half_line)
      )
    )

}

#' @export
#' @rdname ggtheme
theme_minimal <- function(base_size = 12, base_family = "") {
  # Starts with theme_bw and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      axis.ticks        = element_line(),
      axis.ticks.x      = element_blank(),
      axis.ticks.y      = element_blank(),
      axis.ticks.length = unit(1, "lines")
    )
}

#' @export
#' @rdname ggtheme
theme_classic <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black"),
      panel.grid.major   = element_line(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_line(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_rect(colour = "black", size = 0.5),
      legend.key       = element_blank()
    )
}

#' @export
#' @rdname ggtheme
theme_dark <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.ticks        = element_line(colour = "grey40", size = 0.25),
      legend.key        = element_rect(fill = "grey50", colour = "grey40", size = 0.25),
      panel.background  = element_rect(fill = "grey50", colour = NA),
      panel.grid.major  = element_line(colour = "grey40", size = 0.25),
      panel.grid.minor  = element_line(colour = "grey45", size = 0.125),
      strip.background  = element_rect(fill = "grey20", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = -90,
        margin = margin(l = half_line, r = half_line)
      )
    )
}

#' @export
#' @rdname ggtheme
theme_void <- function(base_size = 12, base_family = "") {
  theme(
    # Use only inherited elements and make everything blank
    line =               element_blank(),
    rect =               element_blank(),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),
    plot.margin =        unit(c(0, 0, 0, 0), "lines"),
    axis.text.x =        element_blank(),
    axis.text.y =        element_blank(),
    axis.title.x =       element_blank(),
    axis.title.y =       element_blank(),
    legend.text =        element_text(size = rel(0.8)),
    legend.title =       element_blank(),
    strip.text =         element_text(size = rel(0.8)),

    complete = TRUE
  )
}

