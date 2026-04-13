#' Theme elements
#'
#' @description
#' In conjunction with the \link{theme} system, the `element_` functions
#' specify the display of how non-data components of the plot are drawn.
#'
#'   - `element_blank()`: draws nothing, and assigns no space.
#'   - `element_rect()`: borders and backgrounds.
#'   - `element_line()`: lines.
#'   - `element_text()`: text.
#'   - `element_polygon()`: polygons.
#'   - `element_point()`: points.
#'   - `element_geom()`: defaults for drawing layers.
#'
#' `rel()` is used to specify sizes relative to the parent,
#' `margin()`, `margin_part()` and `margin_auto()` are all used to specify the
#' margins of elements.
#'
#' @param fill Fill colour. `fill_alpha()` can be used to set the transparency
#'   of the fill.
#' @param colour,color Line/border colour. Color is an alias for colour.
#'   `alpha()` can be used to set the transparency of the colour.
#' @param linewidth,borderwidth,stroke Line/border size in mm.
#' @param size,fontsize,pointsize text size in pts, point size in mm.
#' @param linetype,bordertype Line type for lines and borders respectively. An
#'   integer (0:8), a name (blank, solid, dashed, dotted, dotdash, longdash,
#'   twodash), or a string with an even number (up to eight) of hexadecimal
#'   digits which give the lengths in consecutive positions in the string.
#' @param shape,pointshape Shape for points (1-25).
#' @param arrow.fill Fill colour for arrows.
#' @param inherit.blank Should this element inherit the existence of an
#'   `element_blank` among its parents? If `TRUE` the existence of
#'   a blank element among its parents will cause this element to be blank as
#'   well. If `FALSE` any blank parent element will be ignored when
#'   calculating final element state.
#' @param ... Reserved for future expansion.
#'
#' @return An object of class `element`, `rel`, or `margin`.
#' @details
#' The `element_polygon()` and `element_point()` functions are not rendered
#' in standard plots and just serve as extension points.
#'
#' @examples
#' # A standard plot
#' plot <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' # Turning off theme elements by setting them to blank
#' plot + theme(
#'   panel.background = element_blank(),
#'   axis.text = element_blank()
#' )
#'
#' # Text adjustments
#' plot + theme(
#'   axis.text = element_text(colour = "red", size = rel(1.5))
#' )
#'
#' # Turning on the axis line with an arrow
#' plot + theme(
#'   axis.line = element_line(arrow = arrow())
#' )
#'
#' plot + theme(
#'   panel.background = element_rect(fill = "white"),
#'   plot.margin = margin_auto(2, unit = "cm"),
#'   plot.background = element_rect(
#'     fill = "grey90",
#'     colour = "black",
#'     linewidth = 1
#'   )
#' )
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   geom_smooth(formula = y ~ x, method = "lm") +
#'   theme(geom = element_geom(
#'     ink = "red", accent = "black",
#'     pointsize = 1, linewidth = 2
#'   ))
#' @name element
#' @aliases NULL
NULL

#' @export
#' @rdname element
element <- S7::new_class("element", abstract = TRUE)

#' @export
#' @rdname element
element_blank <- S7::new_class(
  "element_blank",
  parent = element,
  constructor = function() {
    obj <- S7::new_object(S7::S7_object())
    class(obj) <- union(
      union(c("ggplot2::element_blank", "element_blank"), class(obj)),
      "element"
    )
    obj
  }
)

# All properties are listed here so they can easily be recycled in the different
# element classes
#' @include properties.R
#' @include margins.R
element_props <- list(
  fill       = property_colour(pattern = TRUE),
  colour     = property_colour(pattern = FALSE),
  family     = property_nullable(S7::class_character),
  hjust      = property_nullable(S7::class_numeric),
  vjust      = property_nullable(S7::class_numeric),
  angle      = property_nullable(S7::class_numeric),
  size       = property_nullable(S7::class_numeric),
  lineheight = property_nullable(S7::class_numeric),
  margin     = property_nullable(margin),
  face       = property_fontface(allow_null = TRUE),
  linewidth  = property_nullable(S7::class_numeric),
  linetype   = property_nullable(S7::class_numeric | S7::class_character),
  lineend    = property_choice(c("round", "butt", "square"), allow_null = TRUE),
  linejoin   = property_choice(c("round", "mitre", "bevel"), allow_null = TRUE),
  shape      = property_nullable(S7::class_numeric | S7::class_character),
  arrow      = property_nullable(S7::new_S3_class("arrow") | S7::class_logical),
  arrow.fill = property_colour(pattern = FALSE),
  debug      = property_boolean(allow_null = TRUE, default = NULL),
  inherit.blank = property_boolean(default = FALSE),
  # These are reserved for future use
  italic     = property_nullable(S7::class_character),
  fontweight = property_nullable(S7::class_numeric),
  fontwidth  = property_nullable(S7::class_numeric | S7::class_character)
)

#' @export
#' @rdname element
element_rect <- S7::new_class(
  "element_rect", parent = element,
  properties = element_props[c("fill", "colour",
                               "linewidth", "linetype", "linejoin",
                               "inherit.blank")],
  constructor = function(fill = NULL, colour = NULL, linewidth = NULL,
                         linetype = NULL, color = NULL, linejoin = NULL,
                         inherit.blank = FALSE, size = deprecated(), ...){
    warn_dots_empty()
    if (lifecycle::is_present(size)) {
      deprecate("3.4.0", "element_rect(size)", "element_rect(linewidth)")
      linewidth <- size
    }
    obj <- S7::new_object(
      S7::S7_object(),
      fill = fill, colour = color %||% colour,
      linewidth = linewidth, linetype = linetype, linejoin = linejoin,
      inherit.blank = inherit.blank
    )
    class(obj) <- union(
      union(c("ggplot2::element_rect", "element_rect"), class(obj)),
      "element"
    )
    obj
  }
)

#' @export
#' @rdname element
#' @param linejoin Line join style, one of `"round"`, `"mitre"` or `"bevel"`.
#' @param lineend Line end style, one of `"round"`, `"butt"` or `"square"`.
#' @param arrow Arrow specification, as created by [grid::arrow()]
element_line <- S7::new_class(
  "element_line", parent = element,
  properties = element_props[c(
    "colour", "linewidth", "linetype", "lineend", "linejoin",
    "arrow", "arrow.fill",
    "inherit.blank"
  )],
  constructor = function(colour = NULL, linewidth = NULL, linetype = NULL,
                         lineend = NULL, color = NULL, linejoin = NULL,
                         arrow = NULL, arrow.fill = NULL,
                         inherit.blank = FALSE, size = deprecated(), ...) {
    warn_dots_empty()
    if (lifecycle::is_present(size)) {
      deprecate("3.4.0", "element_line(size)", "element_line(linewidth)")
      linewidth <- size
    }
    colour <- color %||% colour
    obj <- S7::new_object(
      S7::S7_object(),
      colour = colour,
      linewidth = linewidth, linetype = linetype, lineend = lineend,
      linejoin = linejoin,
      arrow = arrow %||% FALSE,
      arrow.fill = arrow.fill %||% colour,
      inherit.blank = inherit.blank
    )
    class(obj) <- union(
      union(c("ggplot2::element_line", "element_line"), class(obj)),
      "element"
    )
    obj
  }
)

#' @param family The typeface to use. The validity of this value will depend on
#'   the graphics device being used for rendering the plot. See
#'   [the systemfonts vignette](https://systemfonts.r-lib.org/articles/systemfonts.html)
#'   for guidance on the best way to access fonts installed on your computer.
#'   The values `"sans"`, `"serif"`, and `"mono"` should always be valid and
#'   will select the default typeface for the respective styles. However, what
#'   is considered default is dependant on the graphics device and the operating
#'   system.
#' @param face Font face ("plain", "italic", "bold", "bold.italic")
#' @param hjust Horizontal justification (in \eqn{[0, 1]})
#' @param vjust Vertical justification (in \eqn{[0, 1]})
#' @param angle Angle (in \eqn{[0, 360]})
#' @param lineheight Line height
#' @param margin Margins around the text. See [margin()] for more
#'   details. When creating a theme, the margins should be placed on the
#'   side of the text facing towards the center of the plot.
#' @param debug If `TRUE`, aids visual debugging by drawing a solid
#'   rectangle behind the complete text area, and a point where each label
#'   is anchored.
#' @export
#' @rdname element
element_text <- S7::new_class(
  "element_text", parent = element,
  properties = element_props[c(
    "family", "face", "italic", "fontweight", "fontwidth",
    "colour", "size", "hjust", "vjust", "angle", "lineheight",
    "margin", "debug", "inherit.blank"
  )],
  constructor = function(family = NULL, face = NULL, colour = NULL,
                         size = NULL, hjust = NULL, vjust = NULL, angle = NULL,
                         lineheight = NULL, color = NULL, margin = NULL,
                         debug = NULL, inherit.blank = FALSE, ...) {
    warn_dots_empty()
    n <- max(
      length(family), length(face), length(colour), length(size),
      length(hjust), length(vjust), length(angle), length(lineheight)
    )
    if (n > 1) {
      cli::cli_warn(c(
        "Vectorized input to {.fn element_text} is not officially supported.",
        "i" = "Results may be unexpected or may change in future versions of ggplot2."
      ))
    }
    if (!is_margin(margin) && !is.null(margin)) {
      margin <- as_margin(margin)
      cli::cli_warn(
        "The {.arg margin} argument should be constructed using the \\
        {.fn margin} function."
      )
    }

    colour <- color %||% colour
    obj <- S7::new_object(
      S7::S7_object(),
      family = family, face = face, colour = colour, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin, debug = debug, inherit.blank = inherit.blank,
      italic = NA_character_, fontweight = NA_real_, fontwidth  = NA_real_
    )
    class(obj) <- union(
      union(c("ggplot2::element_text", "element_text"), class(obj)),
      "element"
    )
    obj
  }
)

#' @export
#' @rdname element
element_polygon <- S7::new_class(
  "element_polygon", parent = element,
  properties = element_props[c(
    "fill", "colour", "linewidth", "linetype", "linejoin", "inherit.blank"
  )],
  constructor = function(fill = NULL, colour = NULL, linewidth = NULL,
                         linetype = NULL, color = NULL, linejoin = NULL,
                         inherit.blank = FALSE, ...) {
    warn_dots_empty()
    colour <- color %||% colour
    S7::new_object(
      S7::S7_object(),
      fill = fill, colour = color %||% colour, linewidth = linewidth,
      linetype = linetype, linejoin = linejoin, inherit.blank = inherit.blank
    )
  }
)

#' @export
#' @rdname element
element_point <- S7::new_class(
  "element_point", parent = element,
  properties = rename(
    element_props[c(
      "colour", "shape", "size", "fill", "linewidth", "inherit.blank"
    )],
    c("linewidth" = "stroke")
  ),
  constructor = function(colour = NULL, shape = NULL, size = NULL, fill = NULL,
                         stroke = NULL, color = NULL, inherit.blank = FALSE, ...) {
    warn_dots_empty()
    S7::new_object(
      S7::S7_object(),
      colour = color %||% colour, fill = fill, shape = shape, size = size,
      stroke = stroke, inherit.blank = inherit.blank
    )
  }
)

#' @param ink Foreground colour.
#' @param paper Background colour.
#' @param accent Accent colour.
#' @export
#' @rdname element
element_geom <- S7::new_class(
  "element_geom", parent = element,
  properties = list(
    ink = element_props$colour,
    paper = element_props$colour,
    accent = element_props$colour,
    linewidth = element_props$linewidth,
    borderwidth = element_props$linewidth,
    linetype = element_props$linetype,
    bordertype = element_props$linetype,
    family = element_props$family,
    fontsize = element_props$size,
    pointsize = element_props$size,
    pointshape = element_props$shape,
    colour = element_props$colour,
    fill = element_props$fill
  ),
  constructor = function(
    ink = NULL, paper = NULL, accent = NULL,
    linewidth = NULL, borderwidth = NULL,
    linetype = NULL, bordertype = NULL,
    family = NULL, fontsize = NULL,
    pointsize = NULL, pointshape = NULL,
    colour = NULL, color = NULL, fill = NULL,
    ...) {
    warn_dots_empty()
    if (!is.null(fontsize)) {
      fontsize <- fontsize / .pt
    }

    S7::new_object(
      S7::S7_object(),
      ink = ink, paper = paper, accent = accent,
      linewidth = linewidth, borderwidth = borderwidth,
      linetype = linetype, bordertype = bordertype,
      family = family, fontsize = fontsize,
      pointsize = pointsize, pointshape = pointshape,
      colour = color %||% colour, fill = fill
    )
  }
)

.default_geom_element <- element_geom(
  ink = "black", paper = "white", accent = "#3366FF",
  linewidth = 0.5, borderwidth = 0.5,
  linetype = 1L, bordertype = 1L,
  family = "", fontsize = 11,
  pointsize = 1.5, pointshape = 19,
  fill = NULL, colour = NULL
)

local({
  S7::method(print, element) <- function(x, ...) {
    utils::str(x)
  }
})

#' @export
#' @param type For testing elements: the type of element to expect. One of
#'   `"blank"`, `"rect"`, `"line"`, `"text"`, `"polygon"`, `"point"` or `"geom"`.
#' @rdname is_tests
is_theme_element <- function(x, type = "any") {
  switch(
    type %||% "any",
    any     = S7::S7_inherits(x, element),
    blank   = S7::S7_inherits(x, element_blank),
    rect    = S7::S7_inherits(x, element_rect),
    line    = S7::S7_inherits(x, element_line),
    text    = S7::S7_inherits(x, element_text),
    polygon = S7::S7_inherits(x, element_polygon),
    point   = S7::S7_inherits(x, element_point),
    geom    = S7::S7_inherits(x, element_geom),
    FALSE
  )
}

#' @param x A single number specifying size relative to parent element.
#' @rdname element
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

# Element getter methods
local({
  S7::method(`$`, element) <- function(x, i) {
    deprecate("4.1.0", I("`<ggplot2::element>$i`"), I("`<ggplot2::element>@i`"))
    `[[`(S7::props(x), i)
  }
  S7::method(`[`, element) <- function(x, i) {
    deprecate("4.1.0", I("`<gglot2::element>[i]`"), I("`S7::props(<ggplot2::element>, i)`"))
    `[`(S7::props(x), i)
  }
  S7::method(`[[`, element) <- function(x, i) {
    deprecate("4.1.0", I("`<ggplot2::element>[[i]]`"), I("`S7::prop(<ggplot2::element>, i)`"))
    `[[`(S7::props(x), i)
  }
  S7::method(as.list, element) <- function(x, ...) {
    S7::convert(x, S7::class_list)
  }
  S7::method(convert, list(from = element, to = S7::class_list)) <-
    function(from, to, ...) S7::props(from)
  S7::method(
    convert,
    list(
      from = S7::class_list,
      to = element_geom | element_line | element_point |
        element_polygon | element_rect | element_text | element_blank
    )
  ) <- function(from, to, ...) {
    extra <- setdiff(names(from), fn_fmls_names(to))
    if (length(extra) > 0) {
      cli::cli_warn(
        "Unknown {cli::qty(extra)} argument{?s} to {.fn {to@name}}: \\
        {.and {.arg {extra}}}."
      )
      from <- from[setdiff(names(from), extra)]
    }
    inject(to(!!!from))
  }
})

# Element setter methods
#' @export
`$<-.ggplot2::element` <- function(x, i, value) {
  deprecate("4.1.0", I("`<ggplot2::element>$i <- value`"), I("`<ggplot2::element>@i <- value`"))
  S7::props(x) <- `[[<-`(S7::props(x), i, value)
  x
}

#' @export
`[<-.ggplot2::element` <- function(x, i, value) {
  deprecate("4.1.0", I("`<ggplot2::element>[i] <- value`"), I("`S7::props(<ggplot2::element>)[i] <- value`"))
  S7::props(x) <- `[<-`(S7::props(x), i, value)
  x
}

#' @export
`[[<-.ggplot2::element` <- function(x, i, value) {
  deprecate("4.1.0", I("`<ggplot2::element>[[i]] <- value`"), I("S7::prop(<ggplot2::element>, i) <- value"))
  S7::props(x) <- `[[<-`(S7::props(x), i, value)
  x
}

#' @export
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' Reports whether x is a rel object
#' @param x An object to test
#' @keywords internal
is_rel <- function(x) inherits(x, "rel")

#' Render a specified theme element into a grob
#'
#' Given a theme object and element name, returns a grob for the element.
#' Uses [`element_grob()`] to generate the grob.
#' @param theme The theme object
#' @param element The element name given as character vector
#' @param ... Other arguments provided to [`element_grob()`]
#' @param name Character vector added to the name of the grob
#' @keywords internal
#' @export
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    cli::cli_inform("Theme element {.var {element}} is missing")
    return(zeroGrob())
  }

  grob <- element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}

#' Generate grid grob from theme element
#'
#' The `element_grob()` function is vestigial and `draw_element()` should
#' be used instead.
#'
#' @param element Theme element, i.e. `element_rect` or similar.
#' @param ... Other arguments to control specific of rendering. This is
#'   usually at least position. See the source code for individual methods.
#' @keywords internal
#' @export
element_grob <- function(element, ...) {
  # TODO: Swap to S7 generic once S7/#543 is resolved
  UseMethod("element_grob")
}

S7::method(element_grob, element_blank) <- function(element, ...) zeroGrob()

S7::method(element_grob, element_rect) <-
  function(element, x = 0.5, y = 0.5, width = 1, height = 1,
           fill = NULL, colour = NULL,
           linewidth = NULL, linetype = NULL, linejoin = NULL,
           ..., size = deprecated()) {

    if (lifecycle::is_present(size)) {
      deprecate("3.4.0", "element_grob.element_rect(size)", "element_grob.element_rect(linewidth)")
      linewidth <- size
    }

    gp <- gg_par(lwd = linewidth, col = colour, fill = fill, lty = linetype, linejoin = linejoin)
    element_gp <- gg_par(lwd = element@linewidth, col = element@colour,
                         fill = element@fill, lty = element@linetype,
                         linejoin = element@linejoin)

    rectGrob(x, y, width, height, gp = modify_list(element_gp, gp), ...)
  }

S7::method(element_grob, element_text) <-
  function(element, label = "", x = NULL, y = NULL,
           family = NULL, face = NULL, colour = NULL, size = NULL,
           hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
           margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {

    if (is.null(label)) {
      return(zeroGrob())
    }
    if (is_theme_element(label)) {
      cli::cli_warn("{.arg label} cannot be {.obj_type_friendly {label}}.")
      return(zeroGrob())
    }

    vj <- vjust %||% element@vjust
    hj <- hjust %||% element@hjust
    margin <- margin %||% element@margin

    angle <- angle %||% element@angle %||% 0

    # The gp settings can override element_gp
    gp <- gg_par(fontsize = size, col = colour,
                 fontfamily = family, fontface = face,
                 lineheight = lineheight)
    element_gp <- gg_par(fontsize = element@size, col = element@colour,
                         fontfamily = element@family, fontface = element@face,
                         lineheight = element@lineheight)

    titleGrob(label, x, y, hjust = hj, vjust = vj, angle = angle,
              gp = modify_list(element_gp, gp), margin = margin,
              margin_x = margin_x, margin_y = margin_y, debug = element@debug, ...)
  }

S7::method(element_grob, element_line) <-
  function(element, x = 0:1, y = 0:1,
           colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
           linejoin = NULL, arrow.fill = NULL,
           default.units = "npc", id.lengths = NULL, ..., size = deprecated()) {

    if (lifecycle::is_present(size)) {
      deprecate("3.4.0", "element_grob.element_line(size)", "element_grob.element_line(linewidth)")
      linewidth <- size
    }

    arrow <- if (is.logical(element@arrow) && !element@arrow) {
      NULL
    } else {
      element@arrow
    }
    if (is.null(arrow)) {
      arrow.fill <- colour
      element@arrow.fill <- element@colour
    }

    # The gp settings can override element_gp
    gp <- gg_par(
      col = colour, fill = arrow.fill %||% colour,
      lwd = linewidth, lty = linetype, lineend = lineend, linejoin = linejoin
    )
    element_gp <- gg_par(
      col = element@colour, fill = element@arrow.fill %||% element@colour,
      lwd = element@linewidth, lty = element@linetype,
      lineend = element@lineend, linejoin = element@linejoin
    )

    polylineGrob(
      x, y, default.units = default.units,
      gp = modify_list(element_gp, gp),
      id.lengths = id.lengths, arrow = arrow, ...
    )
  }

S7::method(element_grob, element_polygon) <-
  function(element, x = c(0, 0.5, 1, 0.5),
           y = c(0.5, 1, 0.5, 0), fill = NULL,
           colour = NULL, linewidth = NULL,
           linetype = NULL, linejoin = NULL, ...,
           id = NULL, id.lengths = NULL,
           pathId = NULL, pathId.lengths = NULL) {

    gp <- gg_par(lwd = linewidth, col = colour, fill = fill,
                 lty = linetype, linejoin = linejoin)
    element_gp <- gg_par(lwd = element@linewidth, col = element@colour,
                         fill = element@fill, lty = element@linetype,
                         linejoin = element@linejoin)
    pathGrob(
      x = x, y = y, gp = modify_list(element_gp, gp), ...,
      # We swap the id logic so that `id` is always the (super)group id
      # (consistent with `polygonGrob()`) and `pathId` always the subgroup id.
      pathId = id, pathId.lengths = id.lengths,
      id = pathId, id.lengths = pathId.lengths
    )
  }

S7::method(element_grob, element_point) <-
  function(element, x = 0.5, y = 0.5, colour = NULL,
           shape = NULL, fill = NULL, size = NULL,
           stroke = NULL, ...,
           default.units = "npc") {

    gp <- gg_par(col = colour, fill = fill, pointsize = size, stroke = stroke)
    element_gp <- gg_par(col = element@colour, fill = element@fill,
                         pointsize = element@size, stroke = element@stroke)
    shape <- translate_shape_string(shape %||% element@shape %||% 19)
    pointsGrob(x = x, y = y, pch = shape, gp = modify_list(element_gp, gp),
               default.units = default.units, ...)
  }

#' Define and register new theme elements
#'
#' The underlying structure of a ggplot2 theme is defined via the element tree, which
#' specifies for each theme element what type it should have and whether it inherits from
#' a parent element. In some use cases, it may be necessary to modify or extend this
#' element tree and provide default settings for newly defined theme elements.
#'
#' The function `register_theme_elements()` provides the option to globally register new
#' theme elements with ggplot2. In general, for each new theme element both an element
#' definition and a corresponding entry in the element tree should be provided. See
#' examples for details. This function is meant primarily for developers of extension
#' packages, who are strongly urged to adhere to the following best practices:
#'
#' 1. Call `register_theme_elements()` from the `.onLoad()` function of your package, so
#'   that the new theme elements are available to anybody using functions from your package,
#'   irrespective of whether the package has been attached (with `library()` or `require()`)
#'   or not.
#' 2. For any new elements you create, prepend them with the name of your package, to avoid
#'   name clashes with other extension packages. For example, if you are working on a package
#'   **ggxyz**, and you want it to provide a new element for plot panel annotations (as demonstrated
#'   in the Examples below), name the new element `ggxyz.panel.annotation`.
#' @param ... Element specifications
#' @param element_tree Addition of or modification to the element tree, which specifies the
#'   inheritance relationship of the theme elements. The element tree must be provided as
#'   a list of named element definitions created with el_def().
#' @param complete If `TRUE` (the default), elements are set to inherit from blank elements.
#' @seealso
#' The `r link_book("defining theme elements section", "extensions#sec-defining-theme-elements")`
#' @examples
#' # Let's assume a package `ggxyz` wants to provide an easy way to add annotations to
#' # plot panels. To do so, it registers a new theme element `ggxyz.panel.annotation`
#' register_theme_elements(
#'   ggxyz.panel.annotation = element_text(color = "blue", hjust = 0.95, vjust = 0.05),
#'   element_tree = list(ggxyz.panel.annotation = el_def(element_text, "text"))
#' )
#'
#' # Now the package can define a new coord that includes a panel annotation
#' coord_annotate <- function(label = "panel annotation") {
#'   ggproto(NULL, CoordCartesian,
#'     limits = list(x = NULL, y = NULL),
#'     expand = TRUE,
#'     default = FALSE,
#'     clip = "on",
#'     render_fg = function(panel_params, theme) {
#'       element_render(theme, "ggxyz.panel.annotation", label = label)
#'     }
#'   )
#' }
#'
#' # Example plot with this new coord
#' df <- data.frame(x = 1:3, y = 1:3)
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   coord_annotate("annotation in blue")
#'
#' # Revert to the original ggplot2 settings
#' reset_theme_settings()
#' @keywords internal
#' @export
register_theme_elements <- function(..., element_tree = NULL, complete = TRUE) {
  old <- ggplot_global$theme_default
  t <- theme(..., complete = complete)
  ggplot_global$theme_default <- ggplot_global$theme_default %+replace% t

  check_element_tree(element_tree)

  # Merge element trees
  ggplot_global$element_tree <- defaults(element_tree, ggplot_global$element_tree)

  invisible(old)
}

#' @rdname register_theme_elements
#' @details
#' The function `reset_theme_settings()` restores the default element tree, discards
#' all new element definitions, and (unless turned off) resets the currently active
#' theme to the default.
#' @param reset_current If `TRUE` (the default), the currently active theme is
#'   reset to the default theme.
#' @keywords internal
#' @export
reset_theme_settings <- function(reset_current = TRUE) {
  ggplot_global$element_tree <- .element_tree

  # reset the underlying fallback default theme
  ggplot_global$theme_default <- theme_grey()

  if (isTRUE(reset_current)) {
    # reset the currently active theme
    ggplot_global$theme_current <- ggplot_global$theme_default
  }
}

# create the global variables holding all the theme settings
on_load({
  ggplot_global$theme_all_null <- theme_all_null() # cache all null theme, required by theme_grey()
  ggplot_global$theme_current <- NULL  # the current theme applied to plots if none is specified
  ggplot_global$theme_default <- NULL  # the underlying fallback default theme
  ggplot_global$element_tree <- NULL   # the current element tree for themes
  reset_theme_settings() # sets the preceding three global variables to their actual defaults
})

#' @rdname register_theme_elements
#' @details
#' The function `get_element_tree()` returns the currently active element tree.
#' @keywords internal
#' @export
get_element_tree <- function() {
  ggplot_global$element_tree
}

check_element_tree <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_object(x, is_bare_list, "a bare {.cls list}", arg = arg, call = call)
  if (length(x) < 1) {
    return(invisible(NULL))
  }
  check_named(x, arg = arg, call = call)

  # All elements should be constructed with `el_def()`
  fields <- names(el_def())
  bad_fields <- !vapply(x, function(el) all(fields %in% names(el)), logical(1))
  if (any(bad_fields)) {
    bad_fields <- names(x)[bad_fields]
    cli::cli_abort(
      c("{.arg {arg}} must have elements constructed with {.fn el_def}.",
        i = "Invalid structure: {.and {.val {bad_fields}}}"),
      call = call
    )
  }

  # Check element tree, prevent elements from being their own parent (#6162)
  bad_parent <- unlist(Map(
    function(name, el) any(name %in% el$inherit),
    name = names(x), el = x
  ))
  if (any(bad_parent)) {
    bad_parent <- names(x)[bad_parent]
    cli::cli_abort(
      "Invalid parent in {.arg {arg}}: {.and {.val {bad_parent}}}.",
      call = call
    )
  }
  invisible(NULL)
}

#' @rdname register_theme_elements
#' @details
#' The function `el_def()` is used to define new or modified element types and
#' element inheritance relationships for the element tree.
#' @param class The name of the element class. Examples are `element_line` or
#'  `element_text` or "unit", or one of the two reserved keywords "character" or
#'  "margin". The reserved keyword "character" implies a character
#'  or numeric vector, not a class called "character". The keyword
#'  "margin" implies a unit vector of length 4, as created by [margin()].
#' @param inherit A vector of strings, naming the elements that this
#'  element inherits from.
#' @param description An optional character vector providing a description
#'  for the element.
#' @keywords internal
#' @export
el_def <- function(class = NULL, inherit = NULL, description = NULL) {
  if (is.character(class) && length(class) == 1) {
    # Swap S3 class name for S7 class object
    class <- switch(
      class,
      element = element,
      element_blank   = element_blank,
      element_rect    = element_rect,
      element_line    = element_line,
      element_text    = element_text,
      element_polygon = element_polygon,
      element_point   = element_point,
      element_geom    = element_geom,
      margin          = margin,
      class
    )
  }
  # margins often occur in c("unit", "margin", "rel"), we cannot use the
  # S7 class here because we don't support heterogeneous lists
  if (is.character(class) && length(class) > 1) {
    class[class == "margin"] <- "ggplot2::margin"
  }
  list(class = class, inherit = inherit, description = description)
}


# This data structure represents the default theme elements and the inheritance
# among them. It should not be read from directly, since users may modify the
# current element tree stored in ggplot_global$element_tree
.element_tree <- list(
  line                = el_def(element_line),
  rect                = el_def(element_rect),
  text                = el_def(element_text),
  point               = el_def(element_point),
  polygon             = el_def(element_polygon),
  geom                = el_def(element_geom),
  title               = el_def(element_text, "text"),
  spacing             = el_def("unit"),
  margins             = el_def(c("margin", "unit")),

  axis.line           = el_def(element_line, "line"),
  axis.text           = el_def(element_text, "text"),
  axis.title          = el_def(element_text, "title"),
  axis.ticks          = el_def(element_line, "line"),
  legend.key.size     = el_def(c("unit", "rel"), "spacing"),
  panel.grid          = el_def(element_line, "line"),
  panel.grid.major    = el_def(element_line, "panel.grid"),
  panel.grid.minor    = el_def(element_line, "panel.grid"),
  strip.text          = el_def(element_text, "text"),

  axis.line.x         = el_def(element_line, "axis.line"),
  axis.line.x.top     = el_def(element_line, "axis.line.x"),
  axis.line.x.bottom  = el_def(element_line, "axis.line.x"),
  axis.line.y         = el_def(element_line, "axis.line"),
  axis.line.y.left    = el_def(element_line, "axis.line.y"),
  axis.line.y.right   = el_def(element_line, "axis.line.y"),
  axis.line.theta     = el_def(element_line, "axis.line.x"),
  axis.line.r         = el_def(element_line, "axis.line.y"),

  axis.text.x         = el_def(element_text, "axis.text"),
  axis.text.x.top     = el_def(element_text, "axis.text.x"),
  axis.text.x.bottom  = el_def(element_text, "axis.text.x"),
  axis.text.y         = el_def(element_text, "axis.text"),
  axis.text.y.left    = el_def(element_text, "axis.text.y"),
  axis.text.y.right   = el_def(element_text, "axis.text.y"),
  axis.text.theta     = el_def(element_text, "axis.text.x"),
  axis.text.r         = el_def(element_text, "axis.text.y"),

  axis.ticks.length   = el_def(c("unit", "rel"), "spacing"),
  axis.ticks.length.x = el_def(c("unit", "rel"), "axis.ticks.length"),
  axis.ticks.length.x.top = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.x.bottom = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.y  = el_def(c("unit", "rel"), "axis.ticks.length"),
  axis.ticks.length.y.left = el_def(c("unit", "rel"), "axis.ticks.length.y"),
  axis.ticks.length.y.right = el_def(c("unit", "rel"), "axis.ticks.length.y"),
  axis.ticks.length.theta = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.r = el_def(c("unit", "rel"), "axis.ticks.length.y"),

  axis.ticks.x        = el_def(element_line, "axis.ticks"),
  axis.ticks.x.top    = el_def(element_line, "axis.ticks.x"),
  axis.ticks.x.bottom = el_def(element_line, "axis.ticks.x"),
  axis.ticks.y        = el_def(element_line, "axis.ticks"),
  axis.ticks.y.left   = el_def(element_line, "axis.ticks.y"),
  axis.ticks.y.right  = el_def(element_line, "axis.ticks.y"),
  axis.ticks.theta    = el_def(element_line, "axis.ticks.x"),
  axis.ticks.r        = el_def(element_line, "axis.ticks.y"),

  axis.title.x        = el_def(element_text, "axis.title"),
  axis.title.x.top    = el_def(element_text, "axis.title.x"),
  axis.title.x.bottom = el_def(element_text, "axis.title.x"),
  axis.title.y        = el_def(element_text, "axis.title"),
  axis.title.y.left   = el_def(element_text, "axis.title.y"),
  axis.title.y.right  = el_def(element_text, "axis.title.y"),

  axis.minor.ticks.x.top    = el_def(element_line, "axis.ticks.x.top"),
  axis.minor.ticks.x.bottom = el_def(element_line, "axis.ticks.x.bottom"),
  axis.minor.ticks.y.left   = el_def(element_line, "axis.ticks.y.left"),
  axis.minor.ticks.y.right  = el_def(element_line, "axis.ticks.y.right"),
  axis.minor.ticks.theta    = el_def(element_line, "axis.ticks.theta"),
  axis.minor.ticks.r        = el_def(element_line, "axis.ticks.r"),

  axis.minor.ticks.length = el_def(c("unit", "rel")),
  axis.minor.ticks.length.x = el_def(c("unit", "rel"), "axis.minor.ticks.length"),
  axis.minor.ticks.length.x.top = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.x", "axis.ticks.length.x.top")
  ),
  axis.minor.ticks.length.x.bottom = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.x", "axis.ticks.length.x.bottom")
  ),
  axis.minor.ticks.length.y = el_def(c("unit", "rel"), "axis.minor.ticks.length"),
  axis.minor.ticks.length.y.left = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.y", "axis.ticks.length.y.left")
  ),
  axis.minor.ticks.length.y.right = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.y", "axis.ticks.length.y.right")
  ),
  axis.minor.ticks.length.theta = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.x", "axis.ticks.length.theta"),
  ),
  axis.minor.ticks.length.r = el_def(
    c("unit", "rel"), c("axis.minor.ticks.length.y", "axis.ticks.length.r")
  ),

  legend.background   = el_def(element_rect, "rect"),
  legend.margin       = el_def(c("margin", "unit", "rel"), "margins"),
  legend.spacing      = el_def(c("unit", "rel"), "spacing"),
  legend.spacing.x     = el_def(c("unit", "rel"), "legend.spacing"),
  legend.spacing.y     = el_def(c("unit", "rel"), "legend.spacing"),
  legend.key          = el_def(element_rect, "panel.background"),
  legend.key.height   = el_def(c("unit", "rel"), "legend.key.size"),
  legend.key.width    = el_def(c("unit", "rel"), "legend.key.size"),
  legend.key.spacing  = el_def(c("unit", "rel"), "spacing"),
  legend.key.spacing.x = el_def(c("unit", "rel"), "legend.key.spacing"),
  legend.key.spacing.y = el_def(c("unit", "rel"), "legend.key.spacing"),
  legend.key.justification = el_def(c("character", "numeric", "integer")),
  legend.frame        = el_def(element_rect, "rect"),
  legend.axis.line    = el_def(element_line, "line"),
  legend.ticks        = el_def(element_line, "legend.axis.line"),
  legend.ticks.length = el_def(c("rel", "unit"), "legend.key.size"),
  legend.text         = el_def(element_text, "text"),
  legend.text.position = el_def("character"),
  legend.title        = el_def(element_text, "title"),
  legend.title.position = el_def("character"),
  legend.byrow        = el_def("logical"),
  legend.position     = el_def("character"),
  legend.position.inside = el_def(c("numeric", "integer")),
  legend.direction    = el_def("character"),

  legend.justification = el_def(c("character", "numeric", "integer")),
  legend.justification.top = el_def(
    c("character", "numeric", "integer"),
    "legend.justification"
  ),
  legend.justification.bottom = el_def(
    c("character", "numeric", "integer"),
    "legend.justification"
  ),
  legend.justification.left = el_def(
    c("character", "numeric", "integer"),
    "legend.justification"
  ),
  legend.justification.right = el_def(
    c("character", "numeric", "integer"),
    "legend.justification"
  ),
  legend.justification.inside = el_def(
    c("character", "numeric", "integer"),
    "legend.justification"
  ),

  legend.location     = el_def("character"),

  legend.box          = el_def("character"),
  legend.box.just     = el_def("character"),
  legend.box.margin   = el_def(c("margin", "unit", "rel"), "margins"),
  legend.box.background = el_def(element_rect, "rect"),
  legend.box.spacing  = el_def(c("unit", "rel"), "spacing"),

  panel.background    = el_def(element_rect, "rect"),
  panel.border        = el_def(element_rect, "rect"),
  panel.spacing       = el_def(c("unit", "rel"), "spacing"),
  panel.spacing.x     = el_def(c("unit", "rel"), "panel.spacing"),
  panel.spacing.y     = el_def(c("unit", "rel"), "panel.spacing"),
  panel.grid.major.x  = el_def(element_line, "panel.grid.major"),
  panel.grid.major.y  = el_def(element_line, "panel.grid.major"),
  panel.grid.minor.x  = el_def(element_line, "panel.grid.minor"),
  panel.grid.minor.y  = el_def(element_line, "panel.grid.minor"),
  panel.ontop         = el_def("logical"),
  panel.widths        = el_def("unit"),
  panel.heights       = el_def("unit"),

  strip.background    = el_def(element_rect, "rect"),
  strip.background.x  = el_def(element_rect, "strip.background"),
  strip.background.y  = el_def(element_rect, "strip.background"),
  strip.clip          = el_def("character"),
  strip.text.x        = el_def(element_text, "strip.text"),
  strip.text.x.top    = el_def(element_text, "strip.text.x"),
  strip.text.x.bottom = el_def(element_text, "strip.text.x"),
  strip.text.y        = el_def(element_text, "strip.text"),
  strip.text.y.left   = el_def(element_text, "strip.text.y"),
  strip.text.y.right  = el_def(element_text, "strip.text.y"),
  strip.placement     = el_def("character"),
  strip.placement.x   = el_def("character", "strip.placement"),
  strip.placement.y   = el_def("character", "strip.placement"),
  strip.switch.pad.grid = el_def(c("unit", "rel"), "spacing"),
  strip.switch.pad.wrap = el_def(c("unit", "rel"), "spacing"),

  plot.background     = el_def(element_rect, "rect"),
  plot.title          = el_def(element_text, "title"),
  plot.title.position = el_def("character"),
  plot.subtitle       = el_def(element_text, "text"),
  plot.caption        = el_def(element_text, "text"),
  plot.caption.position = el_def("character"),
  plot.tag            = el_def(element_text, "text"),
  plot.tag.position   = el_def(c("character", "numeric", "integer")),  # Need to also accept numbers
  plot.tag.location   = el_def("character"),
  plot.margin         = el_def(c("margin", "unit", "rel"), "margins"),

  palette.colour.discrete   = el_def(c("character", "function")),
  palette.colour.continuous = el_def(c("character", "function")),
  palette.fill.discrete   = el_def(c("character", "function"), "palette.colour.discrete"),
  palette.fill.continuous = el_def(c("character", "function"), "palette.colour.continuous"),
  palette.alpha.discrete   = el_def(c("character", "numeric", "integer", "function")),
  palette.alpha.continuous = el_def(c("character", "numeric", "integer", "function")),
  palette.linewidth.discrete = el_def(c("character", "numeric", "integer", "function")),
  palette.linewidth.continuous = el_def(c("character", "numeric", "integer", "function")),
  palette.size.discrete = el_def(c("character", "numeric", "integer", "function")),
  palette.size.continuous = el_def(c("character", "numeric", "integer", "function")),
  palette.shape.discrete = el_def(c("character", "numeric", "integer", "function")),
  palette.shape.continuous = el_def(c("character", "numeric", "integer", "function")),
  palette.linetype.discrete = el_def(c("character", "numeric", "integer", "function")),
  palette.linetype.continuous = el_def(c("character", "numeric", "integer", "function")),

  aspect.ratio        = el_def(c("numeric", "integer"))
)

# Check that an element object has the proper class
#
# Given an element object and the name of the element, this function
# checks it against the element inheritance tree to make sure the
# element is of the correct class
#
# It throws error if invalid, and returns invisible() if valid.
#
# @param el an element
# @param elname the name of the element
# @param element_tree the element tree to validate against
check_element <- function(el, elname, element_tree, call = caller_env()) {
  eldef <- element_tree[[elname]]

  if (is.null(eldef)) {
    cli::cli_warn("The {.var {elname}} theme element is not defined in the element hierarchy.", call = call)
    return()
  }

  # NULL values for elements are OK
  if (is.null(el)) return()

  class <- eldef$class
  if (inherits(class, "S7_class")) {
    inherit_ok <- S7::S7_inherits(el, class)
    # For backward compatibility
    # TODO: deprecate next release cycle
    old_s3_inherits <- inherits(el, class@name)
    inherit_ok <- inherit_ok || old_s3_inherits
  } else {
    inherit_ok <- inherits(el, class)
  }

  if (is.character(class) && any(c("margin", "ggplot2::margin") %in% class)) {
    if ("rel" %in% class && is_rel(el)) {
      return()
    }
    if (is.unit(el) && length(el) == 4) {
      return()
    }
    cli::cli_abort(
      "The {.var {elname}} theme element must be a {.cls unit} vector of length 4",
      call = call
    )
  }

  # Maybe we should check that `class` is an element class before approving of
  # blank elements?
  if (inherit_ok || is_theme_element(el, "blank")) {
    return()
  }

  class_name <- if (inherits(class, "S7_class")) class@name else class
  cli::cli_abort(
    "The {.var {elname}} theme element must be a {.cls {class_name}} object.",
    call = call
  )
}
