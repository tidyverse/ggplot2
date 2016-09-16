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
#'      colour = factor(gear))) + facet_wrap(~am)
#'
#' p
#' p + theme_bw()
#' p + theme_linedraw()
#' p + theme_light()
#' p + theme_dark()
#' p + theme_minimal()
#' p + theme_classic()
#' p + theme_void()
#' p + theme_gray() # default theme
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

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "grey30", inherit.blank = TRUE),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1, inherit.blank = TRUE),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1, inherit.blank = TRUE),
    axis.ticks =         element_line(colour = "grey20", inherit.blank = TRUE),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
                           margin = margin(t = half_line),
                           vjust = 1, inherit.blank = TRUE
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line),
                           vjust = 1, inherit.blank = TRUE
                         ),

    legend.background =  element_rect(colour = NA, inherit.blank = TRUE),
    legend.spacing =     unit(0.4, "cm"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(0, 0, 0, 0, "cm"),
    legend.key =         element_rect(fill = "grey95", colour = "white", inherit.blank = TRUE),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8), inherit.blank = TRUE),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0, inherit.blank = TRUE),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),

    panel.background =   element_rect(fill = "grey92", colour = NA, inherit.blank = TRUE),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "white", inherit.blank = TRUE),
    panel.grid.minor =   element_line(colour = "white", size = 0.25, inherit.blank = TRUE),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = NA, inherit.blank = TRUE),
    strip.text =         element_text(colour = "grey10", size = rel(0.8), inherit.blank = TRUE),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line), inherit.blank = TRUE),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line), inherit.blank = TRUE),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    element_rect(colour = "white", inherit.blank = TRUE),
    plot.title =         element_text(
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 1.2),
                           inherit.blank = TRUE
                         ),
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 0.9),
                           inherit.blank = TRUE
                         ),
    plot.caption =       element_text(
                           size = rel(0.9),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line * 0.9),
                           inherit.blank = TRUE
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
theme_bw <- function(base_size = 11, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # white background and dark border
      panel.background = element_rect(fill = "white", colour = NA, inherit.blank = TRUE),
      panel.border     = element_rect(fill = NA, colour = "grey20", inherit.blank = TRUE),
      # make gridlines dark, same contrast with white as in theme_grey
      panel.grid.major = element_line(colour = "grey92", inherit.blank = TRUE),
      panel.grid.minor = element_line(colour = "grey92", size = 0.25, inherit.blank = TRUE),
      # contour strips to match panel contour
      strip.background = element_rect(fill = "grey85", colour = "grey20", inherit.blank = TRUE),
      # match legend key to background
      legend.key       = element_rect(fill = "white", colour=NA, inherit.blank = TRUE)
    )
}

#' @export
#' @rdname ggtheme
theme_linedraw <- function(base_size = 11, base_family = "") {
  # Starts with theme_bw and then modify some parts
  # = replace all greys with pure black or white
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # black text and ticks on the axes
      axis.text        = element_text(colour = "black", size = rel(0.8), inherit.blank = TRUE),
      axis.ticks       = element_line(colour = "black", size = 0.25, inherit.blank = TRUE),
      # NB: match the *visual* thickness of axis ticks to the panel border
      #     0.5 clipped looks like 0.25

      # pure black panel border and grid lines, but thinner
      panel.border     = element_rect(fill = NA, colour = "black", size = 0.5, inherit.blank = TRUE),
      panel.grid.major = element_line(colour = "black", size = 0.05, inherit.blank = TRUE),
      panel.grid.minor = element_line(colour = "black", size = 0.025, inherit.blank = TRUE),

      # strips with black background and white text
      strip.background = element_rect(fill = "black", inherit.blank = TRUE),
      strip.text       = element_text(colour = "white", size = rel(0.8), inherit.blank = TRUE)
    )
}

#' @export
#' @rdname ggtheme
theme_light <- function(base_size = 11, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # white panel with light grey border
      panel.background = element_rect(fill = "white", colour = NA, inherit.blank = TRUE),
      panel.border     = element_rect(fill = NA, colour = "grey70", size = 0.5, inherit.blank = TRUE),
      # light grey, thinner gridlines
      # => make them slightly darker to keep acceptable contrast
      panel.grid.major = element_line(colour = "grey87", size = 0.25, inherit.blank = TRUE),
      panel.grid.minor = element_line(colour = "grey87", size = 0.125, inherit.blank = TRUE),

      # match axes ticks thickness to gridlines and colour to panel border
      axis.ticks       = element_line(colour = "grey70", size = 0.25, inherit.blank = TRUE),

      # match legend key to panel.background
      legend.key       = element_rect(fill = "white", colour = NA, inherit.blank = TRUE),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey70", colour = NA, inherit.blank = TRUE),
      strip.text       = element_text(colour = "white", size = rel(0.8), inherit.blank = TRUE)
    )

}

#' @export
#' @rdname ggtheme
theme_dark <- function(base_size = 11, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # dark panel
      panel.background = element_rect(fill = "grey50", colour = NA, inherit.blank = TRUE),
      # inverse grid lines contrast compared to theme_grey
      # make them thinner and try to keep the same visual contrast as in theme_light
      panel.grid.major = element_line(colour = "grey42", size = 0.25, inherit.blank = TRUE),
      panel.grid.minor = element_line(colour = "grey42", size = 0.125, inherit.blank = TRUE),

      # match axes ticks thickness to gridlines
      axis.ticks       = element_line(colour = "grey20", size = 0.25, inherit.blank = TRUE),

      # match legend key to panel.background
      legend.key       = element_rect(fill = "grey50", colour = NA, inherit.blank = TRUE),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey15", colour = NA, inherit.blank = TRUE),
      strip.text       = element_text(colour = "grey90", size = rel(0.8), inherit.blank = TRUE)
    )
}

#' @export
#' @rdname ggtheme
theme_minimal <- function(base_size = 11, base_family = "") {
  # Starts with theme_bw and remove most parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank()
    )
}

#' @export
#' @rdname ggtheme
theme_classic <- function(base_size = 11, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # show axes
      axis.line      = element_line(colour = "black", size = 0.5, inherit.blank = TRUE),

      # match legend key to panel.background
      legend.key       = element_blank(),

      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", size = 1, inherit.blank = TRUE)
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
    )
}

#' @export
#' @rdname ggtheme
theme_void <- function(base_size = 11, base_family = "") {
  theme(
    # Use only inherited elements and make almost everything blank
    # Only keep indispensable text
    line =               element_blank(),
    rect =               element_blank(),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE, inherit.blank = TRUE
                         ),
    axis.text =        element_blank(),
    axis.title =       element_blank(),
    legend.text =        element_text(size = rel(0.8), inherit.blank = TRUE),
    legend.title =       element_text(hjust = 0, inherit.blank = TRUE),
    strip.text =         element_text(size = rel(0.8), inherit.blank = TRUE),
    plot.margin =        unit(c(0, 0, 0, 0), "lines"),

    complete = TRUE
  )
}

