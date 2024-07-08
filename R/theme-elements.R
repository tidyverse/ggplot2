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
#'
#' `rel()` is used to specify sizes relative to the parent,
#' `margin()` is used to specify the margins of elements.
#'
#' @param fill Fill colour.
#' @param colour,color Line/border colour. Color is an alias for colour.
#' @param linewidth Line/border size in mm.
#' @param size text size in pts.
#' @param arrow.fill Fill colour for arrows.
#' @param inherit.blank Should this element inherit the existence of an
#'   `element_blank` among its parents? If `TRUE` the existence of
#'   a blank element among its parents will cause this element to be blank as
#'   well. If `FALSE` any blank parent element will be ignored when
#'   calculating final element state.
#' @return An S3 object of class `element`, `rel`, or `margin`.
#' @examples
#' plot <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' plot + theme(
#'   panel.background = element_blank(),
#'   axis.text = element_blank()
#' )
#'
#' plot + theme(
#'   axis.text = element_text(colour = "red", size = rel(1.5))
#' )
#'
#' plot + theme(
#'   axis.line = element_line(arrow = arrow())
#' )
#'
#' plot + theme(
#'   panel.background = element_rect(fill = "white"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   plot.background = element_rect(
#'     fill = "grey90",
#'     colour = "black",
#'     linewidth = 1
#'   )
#' )
#' @name element
#' @aliases NULL
NULL

#' @export
#' @rdname element
element_blank <- function() {
  structure(
    list(),
    class = c("element_blank", "element")
  )
}

#' @export
#' @rdname element
element_rect <- function(fill = NULL, colour = NULL, linewidth = NULL,
  linetype = NULL, color = NULL, inherit.blank = FALSE, size = deprecated()) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_rect(size)", "element_rect(linewidth)")
    linewidth <- size
  }

  if (!is.null(color))  colour <- color
  structure(
    list(fill = fill, colour = colour, linewidth = linewidth, linetype = linetype,
         inherit.blank = inherit.blank),
    class = c("element_rect", "element")
  )
}

#' @export
#' @rdname element
#' @param linetype Line type. An integer (0:8), a name (blank, solid,
#'    dashed, dotted, dotdash, longdash, twodash), or a string with
#'    an even number (up to eight) of hexadecimal digits which give the
#'    lengths in consecutive positions in the string.
#' @param lineend Line end Line end style (round, butt, square)
#' @param arrow Arrow specification, as created by [grid::arrow()]
element_line <- function(colour = NULL, linewidth = NULL, linetype = NULL,
  lineend = NULL, color = NULL, arrow = NULL, arrow.fill = NULL,
  inherit.blank = FALSE, size = deprecated()) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_line(size)", "element_line(linewidth)")
    linewidth <- size
  }

  colour <- color %||% colour
  arrow.fill <- arrow.fill %||% colour
  arrow <- arrow %||% FALSE

  structure(
    list(colour = colour, linewidth = linewidth, linetype = linetype, lineend = lineend,
      arrow = arrow, arrow.fill = arrow.fill, inherit.blank = inherit.blank),
    class = c("element_line", "element")
  )
}


#' @param family Font family
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
element_text <- function(family = NULL, face = NULL, colour = NULL,
  size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE) {

  if (!is.null(color))  colour <- color

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


  structure(
    list(family = family, face = face, colour = colour, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin, debug = debug, inherit.blank = inherit.blank),
    class = c("element_text", "element")
  )
}


#' @export
print.element <- function(x, ...) utils::str(x)


#' @param x A single number specifying size relative to parent element.
#' @rdname element
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @export
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' Reports whether x is a rel object
#' @param x An object to test
#' @keywords internal
is.rel <- function(x) inherits(x, "rel")

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
#' @param element Theme element, i.e. `element_rect` or similar.
#' @param ... Other arguments to control specific of rendering. This is
#'   usually at least position. See the source code for individual methods.
#' @keywords internal
#' @export
element_grob <- function(element, ...) {
  UseMethod("element_grob")
}

#' @export
element_grob.element_blank <- function(element, ...)  zeroGrob()

#' @export
element_grob.element_rect <- function(element, x = 0.5, y = 0.5,
  width = 1, height = 1,
  fill = NULL, colour = NULL, linewidth = NULL, linetype = NULL, ..., size = deprecated()) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_grob.element_rect(size)", "element_grob.element_rect(linewidth)")
    linewidth <- size
  }

  # The gp settings can override element_gp
  gp <- gg_par(lwd = linewidth, col = colour, fill = fill, lty = linetype)
  element_gp <- gg_par(lwd = element$linewidth, col = element$colour,
    fill = element$fill, lty = element$linetype)

  rectGrob(x, y, width, height, gp = modify_list(element_gp, gp), ...)
}


#' @export
element_grob.element_text <- function(element, label = "", x = NULL, y = NULL,
  family = NULL, face = NULL, colour = NULL, size = NULL,
  hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  margin = NULL, margin_x = FALSE, margin_y = FALSE, ...) {

  if (is.null(label))
    return(zeroGrob())

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust
  margin <- margin %||% element$margin

  angle <- angle %||% element$angle %||% 0

  # The gp settings can override element_gp
  gp <- gg_par(fontsize = size, col = colour,
    fontfamily = family, fontface = face,
    lineheight = lineheight)
  element_gp <- gg_par(fontsize = element$size, col = element$colour,
    fontfamily = element$family, fontface = element$face,
    lineheight = element$lineheight)

  titleGrob(label, x, y, hjust = hj, vjust = vj, angle = angle,
    gp = modify_list(element_gp, gp), margin = margin,
    margin_x = margin_x, margin_y = margin_y, debug = element$debug, ...)
}



#' @export
element_grob.element_line <- function(element, x = 0:1, y = 0:1,
  colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
  arrow.fill = NULL,
  default.units = "npc", id.lengths = NULL, ..., size = deprecated()) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_grob.element_line(size)", "element_grob.element_line(linewidth)")
    linewidth <- size
  }

  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  } else {
    element$arrow
  }
  if (is.null(arrow)) {
    arrow.fill <- colour
    element$arrow.fill <- element$colour
  }

  # The gp settings can override element_gp
  gp <- gg_par(
    col = colour, fill = arrow.fill %||% colour,
    lwd = linewidth, lty = linetype, lineend = lineend
  )
  element_gp <- gg_par(
    col = element$colour, fill = element$arrow.fill %||% element$colour,
    lwd = element$linewidth, lty = element$linetype,
    lineend = element$lineend
  )

  polylineGrob(
    x, y, default.units = default.units,
    gp = modify_list(element_gp, gp),
    id.lengths = id.lengths, arrow = arrow, ...
  )
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
#'   element_tree = list(ggxyz.panel.annotation = el_def("element_text", "text"))
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

#' @rdname register_theme_elements
#' @details
#' The function `el_def()` is used to define new or modified element types and
#' element inheritance relationships for the element tree.
#' @param class The name of the element class. Examples are "element_line" or
#'  "element_text" or "unit", or one of the two reserved keywords "character" or
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
  list(class = class, inherit = inherit, description = description)
}


# This data structure represents the default theme elements and the inheritance
# among them. It should not be read from directly, since users may modify the
# current element tree stored in ggplot_global$element_tree
.element_tree <- list(
  line                = el_def("element_line"),
  rect                = el_def("element_rect"),
  text                = el_def("element_text"),
  title               = el_def("element_text", "text"),
  spacing             = el_def("unit"),
  margins             = el_def(c("margin", "unit")),

  axis.line           = el_def("element_line", "line"),
  axis.text           = el_def("element_text", "text"),
  axis.title          = el_def("element_text", "title"),
  axis.ticks          = el_def("element_line", "line"),
  legend.key.size     = el_def(c("unit", "rel"), "spacing"),
  panel.grid          = el_def("element_line", "line"),
  panel.grid.major    = el_def("element_line", "panel.grid"),
  panel.grid.minor    = el_def("element_line", "panel.grid"),
  strip.text          = el_def("element_text", "text"),

  axis.line.x         = el_def("element_line", "axis.line"),
  axis.line.x.top     = el_def("element_line", "axis.line.x"),
  axis.line.x.bottom  = el_def("element_line", "axis.line.x"),
  axis.line.y         = el_def("element_line", "axis.line"),
  axis.line.y.left    = el_def("element_line", "axis.line.y"),
  axis.line.y.right   = el_def("element_line", "axis.line.y"),
  axis.line.theta     = el_def("element_line", "axis.line.x"),
  axis.line.r         = el_def("element_line", "axis.line.y"),

  axis.text.x         = el_def("element_text", "axis.text"),
  axis.text.x.top     = el_def("element_text", "axis.text.x"),
  axis.text.x.bottom  = el_def("element_text", "axis.text.x"),
  axis.text.y         = el_def("element_text", "axis.text"),
  axis.text.y.left    = el_def("element_text", "axis.text.y"),
  axis.text.y.right   = el_def("element_text", "axis.text.y"),
  axis.text.theta     = el_def("element_text", "axis.text.x"),
  axis.text.r         = el_def("element_text", "axis.text.y"),

  axis.ticks.length   = el_def(c("unit", "rel"), "spacing"),
  axis.ticks.length.x = el_def(c("unit", "rel"), "axis.ticks.length"),
  axis.ticks.length.x.top = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.x.bottom = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.y  = el_def(c("unit", "rel"), "axis.ticks.length"),
  axis.ticks.length.y.left = el_def(c("unit", "rel"), "axis.ticks.length.y"),
  axis.ticks.length.y.right = el_def(c("unit", "rel"), "axis.ticks.length.y"),
  axis.ticks.length.theta = el_def(c("unit", "rel"), "axis.ticks.length.x"),
  axis.ticks.length.r = el_def(c("unit", "rel"), "axis.ticks.length.y"),

  axis.ticks.x        = el_def("element_line", "axis.ticks"),
  axis.ticks.x.top    = el_def("element_line", "axis.ticks.x"),
  axis.ticks.x.bottom = el_def("element_line", "axis.ticks.x"),
  axis.ticks.y        = el_def("element_line", "axis.ticks"),
  axis.ticks.y.left   = el_def("element_line", "axis.ticks.y"),
  axis.ticks.y.right  = el_def("element_line", "axis.ticks.y"),
  axis.ticks.theta    = el_def("element_line", "axis.ticks.x"),
  axis.ticks.r        = el_def("element_line", "axis.ticks.y"),

  axis.title.x        = el_def("element_text", "axis.title"),
  axis.title.x.top    = el_def("element_text", "axis.title.x"),
  axis.title.x.bottom = el_def("element_text", "axis.title.x"),
  axis.title.y        = el_def("element_text", "axis.title"),
  axis.title.y.left   = el_def("element_text", "axis.title.y"),
  axis.title.y.right  = el_def("element_text", "axis.title.y"),

  axis.minor.ticks.x.top    = el_def("element_line", "axis.ticks.x.top"),
  axis.minor.ticks.x.bottom = el_def("element_line", "axis.ticks.x.bottom"),
  axis.minor.ticks.y.left   = el_def("element_line", "axis.ticks.y.left"),
  axis.minor.ticks.y.right  = el_def("element_line", "axis.ticks.y.right"),
  axis.minor.ticks.theta    = el_def("element_line", "axis.ticks.theta"),
  axis.minor.ticks.r        = el_def("element_line", "axis.ticks.r"),

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

  legend.background   = el_def("element_rect", "rect"),
  legend.margin       = el_def(c("margin", "unit", "rel"), "margins"),
  legend.spacing      = el_def(c("unit", "rel"), "spacing"),
  legend.spacing.x     = el_def(c("unit", "rel"), "legend.spacing"),
  legend.spacing.y     = el_def(c("unit", "rel"), "legend.spacing"),
  legend.key          = el_def("element_rect", "panel.background"),
  legend.key.height   = el_def(c("unit", "rel"), "legend.key.size"),
  legend.key.width    = el_def(c("unit", "rel"), "legend.key.size"),
  legend.key.spacing  = el_def(c("unit", "rel"), "spacing"),
  legend.key.spacing.x = el_def(c("unit", "rel"), "legend.key.spacing"),
  legend.key.spacing.y = el_def(c("unit", "rel"), "legend.key.spacing"),
  legend.frame        = el_def("element_rect", "rect"),
  legend.axis.line    = el_def("element_line", "line"),
  legend.ticks        = el_def("element_line", "legend.axis.line"),
  legend.ticks.length = el_def(c("rel", "unit"), "legend.key.size"),
  legend.text         = el_def("element_text", "text"),
  legend.text.position = el_def("character"),
  legend.title        = el_def("element_text", "title"),
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
  legend.box.background = el_def("element_rect", "rect"),
  legend.box.spacing  = el_def(c("unit", "rel"), "spacing"),

  panel.background    = el_def("element_rect", "rect"),
  panel.border        = el_def("element_rect", "rect"),
  panel.spacing       = el_def(c("unit", "rel"), "spacing"),
  panel.spacing.x     = el_def(c("unit", "rel"), "panel.spacing"),
  panel.spacing.y     = el_def(c("unit", "rel"), "panel.spacing"),
  panel.grid.major.x  = el_def("element_line", "panel.grid.major"),
  panel.grid.major.y  = el_def("element_line", "panel.grid.major"),
  panel.grid.minor.x  = el_def("element_line", "panel.grid.minor"),
  panel.grid.minor.y  = el_def("element_line", "panel.grid.minor"),
  panel.ontop         = el_def("logical"),

  strip.background    = el_def("element_rect", "rect"),
  strip.background.x  = el_def("element_rect", "strip.background"),
  strip.background.y  = el_def("element_rect", "strip.background"),
  strip.clip          = el_def("character"),
  strip.text.x        = el_def("element_text", "strip.text"),
  strip.text.x.top    = el_def("element_text", "strip.text.x"),
  strip.text.x.bottom = el_def("element_text", "strip.text.x"),
  strip.text.y        = el_def("element_text", "strip.text"),
  strip.text.y.left   = el_def("element_text", "strip.text.y"),
  strip.text.y.right  = el_def("element_text", "strip.text.y"),
  strip.placement     = el_def("character"),
  strip.placement.x   = el_def("character", "strip.placement"),
  strip.placement.y   = el_def("character", "strip.placement"),
  strip.switch.pad.grid = el_def(c("unit", "rel"), "spacing"),
  strip.switch.pad.wrap = el_def(c("unit", "rel"), "spacing"),

  plot.background     = el_def("element_rect", "rect"),
  plot.title          = el_def("element_text", "title"),
  plot.title.position = el_def("character"),
  plot.subtitle       = el_def("element_text", "title"),
  plot.caption        = el_def("element_text", "title"),
  plot.caption.position = el_def("character"),
  plot.tag            = el_def("element_text", "title"),
  plot.tag.position   = el_def(c("character", "numeric", "integer")),  # Need to also accept numbers
  plot.tag.location   = el_def("character"),
  plot.margin         = el_def(c("margin", "unit", "rel"), "margins"),

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
validate_element <- function(el, elname, element_tree, call = caller_env()) {
  eldef <- element_tree[[elname]]

  if (is.null(eldef)) {
    cli::cli_warn("The {.var {elname}} theme element is not defined in the element hierarchy.", call = call)
    return()
  }

  # NULL values for elements are OK
  if (is.null(el)) return()

  if ("margin" %in% eldef$class) {
    if (!is.unit(el) && length(el) == 4)
      cli::cli_abort("The {.var {elname}} theme element must be a {.cls unit} vector of length 4.", call = call)
  } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
    cli::cli_abort("The {.var {elname}} theme element must be a {.cls {eldef$class}} object.", call = call)
  }
  invisible()
}
