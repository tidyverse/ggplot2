#' Complete themes
#'
#' These are complete themes which control all non-data display. Use
#' [theme()] if you just need to tweak the display of an existing
#' theme.
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param header_family font family for titles and headers. The default, `NULL`,
#'   uses theme inheritance to set the font. This setting affects axis titles,
#'   legend titles, the plot title and tag text.
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param ink,paper colour for foreground and background elements respectively.
#'
#' @details
#' \describe{
#'
#' \item{`theme_gray()`}{
#' The signature ggplot2 theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.}
#'
#' \item{`theme_bw()`}{
#' The classic dark-on-light ggplot2 theme. May work better for presentations
#' displayed with a projector.}
#'
#' \item{`theme_linedraw()`}{
#' A theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawing. Serves a purpose similar to `theme_bw()`.
#' Note that this theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{`theme_light()`}{
#' A theme similar to `theme_linedraw()` but with light grey lines and axes,
#' to direct more attention towards the data.}
#'
#' \item{`theme_dark()`}{
#' The dark cousin of `theme_light()`, with similar line sizes but a dark background. Useful to make thin coloured lines pop out.}
#'
#' \item{`theme_minimal()`}{
#' A minimalistic theme with no background annotations.}
#'
#' \item{`theme_classic()`}{
#' A classic-looking theme, with x and y axis lines and no gridlines.}
#'
#' \item{`theme_void()`}{
#' A completely empty theme.}
#'
#' \item{`theme_test()`}{
#' A theme for visual unit tests. It should ideally never change except
#' for new features.}
#'
#' }
#'
#' @seealso
#' The `r link_book("complete themes section", "themes#sec-themes")`
#'
#' @examples
#' mtcars2 <- within(mtcars, {
#'   vs <- factor(vs, labels = c("V-shaped", "Straight"))
#'   am <- factor(am, labels = c("Automatic", "Manual"))
#'   cyl  <- factor(cyl)
#'   gear <- factor(gear)
#' })
#'
#' p1 <- ggplot(mtcars2) +
#'   geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'   labs(
#'     title = "Fuel economy declines as weight increases",
#'     subtitle = "(1973-74)",
#'     caption = "Data from the 1974 Motor Trend US magazine.",
#'     tag = "Figure 1",
#'     x = "Weight (1000 lbs)",
#'     y = "Fuel economy (mpg)",
#'     colour = "Gears"
#'   )
#'
#' p1 + theme_gray() # the default
#' p1 + theme_bw()
#' p1 + theme_linedraw()
#' p1 + theme_light()
#' p1 + theme_dark()
#' p1 + theme_minimal()
#' p1 + theme_classic()
#' p1 + theme_void()
#'
#' # Theme examples with panels
#' \donttest{
#' p2 <- p1 + facet_grid(vs ~ am)
#'
#' p2 + theme_gray() # the default
#' p2 + theme_bw()
#' p2 + theme_linedraw()
#' p2 + theme_light()
#' p2 + theme_dark()
#' p2 + theme_minimal()
#' p2 + theme_classic()
#' p2 + theme_void()
#' }
#' @name ggtheme
#' @aliases NULL
NULL

#' @include theme.R
#' @export
#' @rdname ggtheme
theme_grey <- function(base_size = 11, base_family = "",
                       header_family = NULL,
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       ink = "black", paper = "white") {

  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size/2`
  # instead.
  half_line <- base_size / 2

  # Throughout the theme, we use three font sizes, `base_size` (`rel(1)`)
  # for normal, `rel(0.8)` for small, and `rel(1.2)` for large.

  t <- theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(
                           colour = ink, linewidth = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = paper, colour = ink,
                           linewidth = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = ink, size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    title =              element_text(family = header_family),

    spacing = unit(half_line, "pt"),
    margins = margin_auto(half_line),

    geom =               element_geom(
                           ink = ink, paper = paper, accent = "#3366FF",
                           linewidth = base_line_size, borderwidth = base_line_size,
                           linetype = 1L, bordertype = 1L,
                           family = base_family, fontsize = base_size,
                           pointsize = (base_size / 11) * 1.5, pointshape = 19
                         ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = col_mix(ink, paper, 0.305)),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.text.r =        element_text(margin = margin(l = 0.8 * half_line / 2, r = 0.8 * half_line / 2),
                                      hjust = 0.5),
    axis.ticks =         element_line(colour = col_mix(ink, paper, 0.2)),
    axis.ticks.length =  rel(0.5),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = rel(0.75),
    axis.title.x =       element_text(
                           margin = margin(t = half_line / 2),
                           vjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line / 2),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line / 2),
                           vjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line / 2),
                           vjust = 1
                         ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     rel(2),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      NULL,
    legend.key =         NULL,
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.key.spacing = NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.title =       element_text(hjust = 0),
    legend.ticks.length = rel(0.2),
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  rel(0),
    legend.box.background = element_blank(),
    legend.box.spacing = rel(2),

    panel.background =   element_rect(fill = col_mix(ink, paper, 0.925), colour = NA),
    panel.border =       element_blank(),
    panel.grid =         element_line(colour = paper),
    panel.grid.minor =   element_line(linewidth = rel(0.5)),
    panel.spacing =      NULL,
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = col_mix(ink, paper, 0.854), colour = NA),
    strip.clip =         "on",
    strip.text =         element_text(
                           colour = col_mix(ink, paper, 0.105),
                           size = rel(0.8),
                           margin = margin_auto(0.8 * half_line)
                         ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background =    element_rect(colour = paper),
    plot.title =         element_text( # font size "large"
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      element_text( # font size "regular"
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.caption =       element_text( # font size "small"
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    plot.margin =        NULL,

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined
  ggplot_global$theme_all_null %+replace% t
}
#' @export
#' @rdname ggtheme
theme_gray <- theme_grey

#' @export
#' @rdname ggtheme
theme_bw <- function(base_size = 11, base_family = "",
                     header_family = NULL,
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22,
                     ink = "black", paper = "white") {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      # white background and dark border
      panel.background = element_rect(fill = paper, colour = NA),
      panel.border     = element_rect(colour = col_mix(ink, paper, 0.2)),
      # make gridlines dark, same contrast with white as in theme_grey
      panel.grid = element_line(colour = col_mix(ink, paper, 0.925)),
      panel.grid.minor = element_line(linewidth = rel(0.5)),
      # contour strips to match panel contour
      strip.background = element_rect(
        fill   = col_mix(ink, paper, 0.851),
        colour = col_mix(ink, paper, 0.2)
      ),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_linedraw <- function(base_size = 11, base_family = "",
                           header_family = NULL,
                           base_line_size = base_size / 22,
                           base_rect_size = base_size / 22,
                           ink = "black", paper = "white") {
  half_line <- base_size / 2

  # Starts with theme_bw and then modify some parts
  # = replace all greys with pure black or white
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      # black text and ticks on the axes
      axis.text        = element_text(colour = ink, size = rel(0.8)),
      axis.ticks       = element_line(colour = ink, linewidth = rel(0.5)),
      # NB: match the *visual* thickness of axis ticks to the panel border
      #     0.5 clipped looks like 0.25

      # pure black panel border and grid lines, but thinner
      panel.border     = element_rect(colour = ink, linewidth = rel(1)),
      panel.grid       = element_line(colour = ink),
      panel.grid.major = element_line(linewidth = rel(0.1)),
      panel.grid.minor = element_line(linewidth = rel(0.05)),

      # strips with black background and white text
      strip.background = element_rect(fill = ink),
      strip.text       = element_text(
                           colour = paper,
                           size = rel(0.8),
                           margin = margin_auto(0.8 * half_line)
                         ),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_light <- function(base_size = 11, base_family = "",
                        header_family = NULL,
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22,
                        ink = "black", paper = "white") {
  half_line <- base_size / 2

  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      # white panel with light grey border
      panel.background = element_rect(fill = paper, colour = NA),
      panel.border     = element_rect(colour = col_mix(ink, paper, 0.705), linewidth = rel(1)),
      # light grey, thinner gridlines
      # => make them slightly darker to keep acceptable contrast
      panel.grid       = element_line(colour = col_mix(ink, paper, 0.871)),
      panel.grid.major = element_line(linewidth = rel(0.5)),
      panel.grid.minor = element_line(linewidth = rel(0.25)),

      # match axes ticks thickness to gridlines and colour to panel border
      axis.ticks       = element_line(colour = col_mix(ink, paper, 0.705), linewidth = rel(0.5)),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = col_mix(ink, paper, 0.705), colour = NA),
      strip.text       = element_text(
                           colour = paper,
                           size = rel(0.8),
                           margin = margin_auto(0.8 * half_line)
                         ),

      complete = TRUE
    )

}

#' @export
#' @rdname ggtheme
theme_dark <- function(base_size = 11, base_family = "",
                       header_family = NULL,
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       ink = "black", paper = "white") {
  half_line <- base_size / 2

  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      # dark panel
      panel.background = element_rect(fill = col_mix(ink, paper, 0.5), colour = NA),
      # inverse grid lines contrast compared to theme_grey
      # make them thinner and try to keep the same visual contrast as in theme_light
      panel.grid       = element_line(colour = col_mix(ink, paper, 0.42)),
      panel.grid.major = element_line(linewidth = rel(0.5)),
      panel.grid.minor = element_line(linewidth = rel(0.25)),

      # match axes ticks thickness to gridlines
      axis.ticks       = element_line(colour = col_mix(ink, paper, 0.2), linewidth = rel(0.5)),

      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = col_mix(ink, paper, 0.15), colour = NA),
      strip.text       = element_text(
                           colour = col_mix(ink, paper, 0.9),
                           size = rel(0.8),
                           margin = margin_auto(0.8 * half_line)
                         ),

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_minimal <- function(base_size = 11, base_family = "",
                          header_family = NULL,
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22,
                          ink = "black", paper = "white") {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      axis.ticks         = element_blank(), # Extra margins due to absence ticks
      axis.text.x.bottom = element_text(margin = margin(t = 0.45 * base_size)),
      axis.text.x.top    = element_text(margin = margin(b = 0.45 * base_size)),
      axis.text.y.left   = element_text(margin = margin(r = 0.45 * base_size)),
      axis.text.y.right  = element_text(margin = margin(l = 0.45 * base_size)),
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
                          header_family = NULL,
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22,
                          ink = "black", paper = "white") {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # show axes
      axis.text  = element_text(size = rel(0.8)),
      axis.line  = element_line(lineend = "square"),
      axis.ticks = element_line(),

      # simple, black and white strips
      strip.background = element_rect(linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes

      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_transparent <- function(base_size = 11, base_family = "",
                              header_family = NULL,
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22,
                              ink = "black", paper = alpha(ink, 0)) {
  force(ink)
  # Based on theme_bw
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink, paper = paper
  ) %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background   = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      # theme_bw specifications
      panel.border      = element_rect(fill = NA, colour = col_mix(ink, paper, 0.20)),
      # make gridlines dark, same contrast with white as in theme_grey
      panel.grid        = element_line(colour = col_mix(ink, paper, 0.92)),
      # contour strips to match panel contour
      strip.background  = element_rect(
        fill = col_mix(ink, paper, 0.85),
        colour = col_mix(ink, paper, 0.20),
      ),
      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
theme_void <- function(base_size = 11, base_family = "",
                       header_family = NULL,
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       ink = "black", paper = "white") {
  half_line <- base_size / 2

  # Only keep indispensable text: legend and plot titles
  t <- theme(
    line =               element_blank(),
    rect =               element_blank(),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = ink, size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),
    title =              element_text(family = header_family),
    spacing =            unit(half_line, "pt"),
    margins =            margin_auto(half_line),
    axis.text =          element_blank(),
    axis.title =         element_blank(),
    axis.ticks.length =  rel(0),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = NULL,
    legend.box =         NULL,
    legend.key.size =    unit(1.2, "lines"),
    legend.position =    "right",
    legend.text =        element_text(size = rel(0.8)),
    legend.title =       element_text(hjust = 0),
    legend.key.spacing = rel(1),
    legend.margin =      rel(0),
    legend.box.margin =  rel(0),
    legend.box.spacing = unit(0.2, "cm"),
    legend.ticks.length = rel(0.2),
    strip.clip =         "on",
    strip.text =         element_text(size = rel(0.8)),
    strip.switch.pad.grid = rel(0.5),
    strip.switch.pad.wrap = rel(0.5),
    panel.ontop =        FALSE,
    panel.spacing =      NULL,
    plot.margin =        rel(0),
    plot.title =         element_text(
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      element_text(
                           hjust = 0, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption =       element_text(
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined
  ggplot_global$theme_all_null %+replace% t
}


#' @export
#' @rdname ggtheme
theme_test <- function(base_size = 11, base_family = "",
                       header_family = NULL,
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       ink = "black", paper = "white") {
  half_line <- base_size / 2

  t <- theme(
    line =               element_line(
                           colour = ink, linewidth = base_line_size,
                           linetype = 1, lineend = "butt"
                         ),
    rect =               element_rect(
                           fill = paper, colour = ink,
                           linewidth = base_rect_size, linetype = 1
                         ),
    text =               element_text(
                            family = base_family, face = "plain",
                            colour = ink, size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),
    title =              element_text(family = header_family),
    spacing = unit(half_line, "pt"),
    margins = margin_auto(half_line),
    geom =               element_geom(
                           ink = ink, paper = paper, accent = "#3366FF",
                           linewidth = base_line_size, borderwidth = base_line_size,
                           family = base_family, fontsize = base_size,
                           linetype = 1L,
                           pointsize = (base_size / 11) * 1.5, pointshape = 19
                         ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = col_mix(ink, paper, 0.305)),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = col_mix(ink, paper, 0.2)),
    axis.ticks.length =  rel(0.5),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = rel(0.75),
    axis.title.x =       element_text(
                           margin = margin(t = half_line / 2),
                           vjust = 1
                         ),
    axis.title.x.top =   element_text(
                           margin = margin(b = half_line / 2),
                           vjust = 0
                         ),
    axis.title.y =       element_text(
                           angle = 90,
                           margin = margin(r = half_line / 2),
                           vjust = 1
                         ),
    axis.title.y.right = element_text(
                           angle = -90,
                           margin = margin(l = half_line / 2),
                           vjust = 1
                         ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     rel(2),
    legend.spacing.x =   NULL,
    legend.spacing.y =   NULL,
    legend.margin =      margin_auto(0, unit = "cm"),
    legend.key =         NULL,
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.key.spacing = NULL,
    legend.key.spacing.x = NULL,
    legend.key.spacing.y = NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.title =       element_text(hjust = 0),
    legend.ticks.length = rel(0.2),
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin_auto(0, unit = "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = rel(2),

    panel.background =   element_rect(fill = paper, colour = NA),
    panel.border =       element_rect(colour = col_mix(ink, paper, 0.2)),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      NULL,
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(
                           fill   = col_mix(ink, paper, 0.851),
                           colour = col_mix(ink, paper, 0.2)
                         ),
    strip.clip =         "on",
    strip.text =         element_text(
                           colour = col_mix(ink, paper, 0.105),
                           size = rel(0.8),
                           margin = margin_auto(0.8 * half_line)
                         ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = rel(0.5),
    strip.switch.pad.wrap = rel(0.5),

    plot.background =    element_rect(colour = paper),
    plot.title =         element_text(
                           size = rel(1.2),
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.title.position = "panel",
    plot.subtitle =      element_text(
                           hjust = 0, vjust = 1,
                           margin = margin(b = half_line)
                         ),
    plot.caption =       element_text(
                           size = rel(0.8),
                           hjust = 1, vjust = 1,
                           margin = margin(t = half_line)
                         ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
                           size = rel(1.2),
                           hjust = 0.5, vjust = 0.5
                         ),
    plot.tag.position =  'topleft',
    plot.margin =        NULL,

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined
  ggplot_global$theme_all_null %+replace% t
}

theme_all_null <- function() {
  # Set all elements in the element tree to NULL.

  # We read from `.element_tree` instead of `ggplot_global$element_tree`
  # because we don't want to change our results just because a user
  # has defined new theme elements.
  elements <- sapply(
    names(.element_tree),
    function(x) NULL,
    simplify = FALSE, USE.NAMES = TRUE
  )

  args <- c(elements, list(complete = TRUE))
  inject(theme(!!!args))
}
