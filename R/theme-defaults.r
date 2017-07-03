#' Complete themes
#'
#' These are complete themes which control all non-data display. Use
#' [theme()] if you just need to tweak the display of an existing
#' theme.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @details
#' \describe{
#'
#' \item{`theme_gray`}{
#' The signature ggplot2 theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.}
#'
#' \item{`theme_bw`}{
#' The classic dark-on-light ggplot2 theme. May work better for presentations
#' displayed with a projector.}
#'
#' \item{`theme_linedraw`}{
#' A theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawings. Serves a purpose similar to `theme_bw`.
#' Note that this theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{`theme_light`}{
#' A theme similar to `theme_linedraw` but with light grey lines and axes,
#' to direct more attention towards the data.}
#'
#' \item{`theme_dark`}{
#' The dark cousin of `theme_light`, with similar line sizes but a dark background. Useful to make thin coloured lines pop out.}
#'
#' \item{`theme_minimal`}{
#' A minimalistic theme with no background annotations.}
#'
#' \item{`theme_classic`}{
#' A classic-looking theme, with x and y axis lines and no gridlines.}
#'
#' \item{`theme_void`}{
#' A completely empty theme.}
#'
#' \item{`theme_test`}{
#' A theme for visual unit tests. It should ideally never change except
#' for new features.}
#'
#' }
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour = factor(gear))) + facet_wrap(~am)
#' p + theme_gray() # the default
#' p + theme_bw()
#' p + theme_linedraw()
#' p + theme_light()
#' p + theme_dark()
#' p + theme_minimal()
#' p + theme_classic()
#' p + theme_void()
#' @name ggtheme
#' @aliases NULL
NULL

#' @include theme.r
#' @export
#' @rdname ggtheme
theme_grey <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  half_line <- base_size / 2

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(
                           colour = "black", size = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = "white", colour = "black",
                           size = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "grey20"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
                           margin = margin(t = half_line),
                           vjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line),
                           vjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line),
                           vjust = 0
                         ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     unit(0.4, "cm"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(0.2, 0.2, 0.2, 0.2, "cm"),
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
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),

    panel.background =   element_rect(fill = "grey92", colour = NA),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "white"),
    panel.grid.minor =   element_line(colour = "white", size = rel(0.5)),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 1.2)
                         ),
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 0.9)
                         ),
    plot.caption =       element_text(
                           size = rel(0.9),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line * 0.9)
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
theme_bw <- function(base_size = 11, base_family = "",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # white background and dark border
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey20"),
      # make gridlines dark, same contrast with white as in theme_grey
      panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_line(colour = "grey92", size = rel(0.5)),
      # contour strips to match panel contour
      strip.background = element_rect(fill = "grey85", colour = "grey20"),
      # match legend key to background
      legend.key       = element_rect(fill = "white", colour = NA),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_linedraw <- function(base_size = 11, base_family = "",
                           base_line_size = base_size / 22,
                           base_rect_size = base_size / 22) {
  # Starts with theme_bw and then modify some parts
  # = replace all greys with pure black or white
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # black text and ticks on the axes
      axis.text        = element_text(colour = "black", size = rel(0.8)),
      axis.ticks       = element_line(colour = "black", size = rel(0.5)),
      # NB: match the *visual* thickness of axis ticks to the panel border
      #     0.5 clipped looks like 0.25

      # pure black panel border and grid lines, but thinner
      panel.border     = element_rect(fill = NA, colour = "black", size = rel(1)),
      panel.grid.major = element_line(colour = "black", size = rel(0.1)),
      panel.grid.minor = element_line(colour = "black", size = rel(0.05)),

      # strips with black background and white text
      strip.background = element_rect(fill = "black"),
      strip.text       = element_text(colour = "white", size = rel(0.8)),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_light <- function(base_size = 11, base_family = "",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22) {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # white panel with light grey border
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey70", size = rel(1)),
      # light grey, thinner gridlines
      # => make them slightly darker to keep acceptable contrast
      panel.grid.major = element_line(colour = "grey87", size = rel(0.5)),
      panel.grid.minor = element_line(colour = "grey87", size = rel(0.25)),

      # match axes ticks thickness to gridlines and colour to panel border
      axis.ticks       = element_line(colour = "grey70", size = rel(0.5)),

      # match legend key to panel.background
      legend.key       = element_rect(fill = "white", colour = NA),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey70", colour = NA),
      strip.text       = element_text(colour = "white", size = rel(0.8)),

      complete = TRUE
    )

}

#' @export
#' @rdname ggtheme
theme_dark <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # dark panel
      panel.background = element_rect(fill = "grey50", colour = NA),
      # inverse grid lines contrast compared to theme_grey
      # make them thinner and try to keep the same visual contrast as in theme_light
      panel.grid.major = element_line(colour = "grey42", size = rel(0.5)),
      panel.grid.minor = element_line(colour = "grey42", size = rel(0.25)),

      # match axes ticks thickness to gridlines
      axis.ticks       = element_line(colour = "grey20", size = rel(0.5)),

      # match legend key to panel.background
      legend.key       = element_rect(fill = "grey50", colour = NA),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey15", colour = NA),
      strip.text       = element_text(colour = "grey90", size = rel(0.8)),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_minimal <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_classic <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # show axes
      axis.line      = element_line(colour = "black", size = rel(1)),

      # match legend key to panel.background
      legend.key       = element_blank(),

      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", size = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_void <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  theme(
    # Use only inherited elements and make almost everything blank
    # Only keep indispensable text
    line =               element_blank(),
    rect =               element_blank(),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),
    axis.text =        element_blank(),
    axis.title =       element_blank(),
    legend.text =        element_text(size = rel(0.8)),
    legend.title =       element_text(hjust = 0),
    strip.text =         element_text(size = rel(0.8)),
    plot.margin =        unit(c(0, 0, 0, 0), "lines"),

    complete = TRUE
  )
}

#' @export
#' @rdname ggtheme
theme_test <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  half_line <- base_size / 2
  
  theme(
    line =               element_line(
                           colour = "black", size = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = "white", colour = "black",
                           size = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "grey20"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
                           margin = margin(t = half_line),
                           vjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line),
                           vjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line),
                           vjust = 0
                         ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     unit(0.4, "cm"),
    legend.spacing.x =   NULL,
    legend.spacing.y =   NULL,
    legend.margin =      margin(0, 0, 0, 0, "cm"),
    legend.key =         element_rect(fill = "white", colour=NA),
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
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(0.4, "cm"),

    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_rect(fill = NA, colour = "grey20"),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = "grey20"),
    strip.text =         element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 1.2)
                         ),
    plot.subtitle =      element_text(
                           size = rel(0.9),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line * 0.9)
                         ),
    plot.caption =       element_text(
                           size = rel(0.9),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line * 0.9)
                         ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
