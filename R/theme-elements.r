#' Theme element: blank.
#' This theme element draws nothing, and assigns no space
#' 
#' @export
element_blank <- function() {
  structure(
    list(),
    class = c("element_blank", "element")
  )  
}

#' Theme element: rectangle.
#' 
#' Most often used for backgrounds and borders.
#' 
#' @param fill fill colour
#' @param colour border colour
#' @param size border size
#' @param linetype border linetype
#' @param color an alias for \code{colour}
#' @export
element_rect <- function(fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, color = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(fill = fill, colour = colour, size = size, linetype = linetype),
    class = c("element_rect", "element")
  )
}

#' Theme element: line.
#' 
#' @param colour line colour
#' @param size line size
#' @param linetype line type
#' @param lineend line end
#' @param color an alias for \code{colour}
#' @export
element_line <- function(colour = NULL, size = NULL, linetype = NULL,
  lineend = NULL, color = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(colour = colour, size = size, linetype = linetype, lineend = lineend),
    class = c("element_line", "element")
  )
}


#' Theme element: text.
#' 
#' @param family font family
#' @param face font face ("plain", "italic", "bold", "bold.italic")
#' @param colour text colour
#' @param size text size (in pts)
#' @param hjust horizontal justification (in [0, 1])
#' @param vjust vertical justification (in [0, 1])
#' @param angle angle (in [0, 360])
#' @param lineheight line height
#' @param color an alias for \code{colour}
#' @export
element_text <- function(family = NULL, face = NULL, colour = NULL,
  size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  color = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(family = family, face = face, colour = colour, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight),
    class = c("element_text", "element")
  )
}


#' @S3method print element
print.element <- function(x, ...) str(x)


#' Relative sizing for theme elements
#'
#' @param x A number representing the relative size
#' @examples
#' qplot(1:3, 1:3) + theme(axis.title.x = element_text(size = rel(2.5)))
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @S3method print rel
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' Reports whether x is a rel object
#' @param x An object to test
is.rel <- function(x) inherits(x, "rel")


#' Deprecated theme_xx functions
#'
#' The \code{theme_xx} functions have been deprecated. They are replaced
#' with the \code{element_xx} functions.
#' @param ... Arguments to be passed to the appropriate \code{element_xx}
#'   function.
#'
#' @export
theme_blank <- function(...) {
  gg_dep("0.9.1", "'theme_blank' is deprecated. Use 'element_blank' instead.")
  element_blank(...)
}

#' @rdname theme_blank
#' @export
theme_rect <- function(...) {
  gg_dep("0.9.1", "theme_rect is deprecated. Use 'element_rect' instead.")
  element_rect(...)
}

#' @rdname theme_blank
#' @export
theme_line <- function(...) {
  gg_dep("0.9.1", "theme_line is deprecated. Use 'element_line' instead.")
  element_line(...)
}

#' @rdname theme_blank
#' @export
theme_segment <- function(...) {
  gg_dep("0.9.1", "theme_segment is deprecated. Use 'element_line' instead.")
  element_line(...)
}

#' @rdname theme_blank
#' @export
theme_text <- function(...) {
  gg_dep("0.9.1", "theme_text is deprecated. Use 'element_text' instead.")
  element_text(...)
}


# Given a theme object and element name, return a grob for the element
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  ggname(ps(element, name, sep = "."), element_grob(el, ...))
}


# Returns NULL if x is length 0
len0_null <- function(x) {
  if (length(x) == 0)  NULL
  else                 x
}


# Returns a grob for an element object
element_grob <- function(element, ...)
  UseMethod("element_grob")


#' @S3method element_grob element_blank
element_grob.element_blank <- function(element, ...)  zeroGrob()

#' @S3method element_grob element_rect
element_grob.element_rect <- function(element, x = 0.5, y = 0.5,
  width = 1, height = 1,
  fill = NULL, colour = NULL, size = NULL, linetype = NULL, ...) {

  # The gp settings can override element_gp
  gp <- gpar(lwd = len0_null(size * .pt), col = colour, fill = fill, lty = linetype)
  element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour,
    fill = element$fill, lty = element$linetype)

  rectGrob(x, y, width, height, gp = modifyList(element_gp, gp), ...)
}


#' @S3method element_grob element_text
element_grob.element_text <- function(element, label = "", x = NULL, y = NULL,
  family = NULL, face = NULL, colour = NULL, size = NULL,
  hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  default.units = "npc", ...) {

  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust

  angle <- angle %||% element$angle
  if (is.null(angle)) {
    stop("Text element requires non-NULL value for 'angle'.")
  }
  angle <- angle %% 360
  
  if (angle == 90) {
    xp <- vj
    yp <- hj
  } else if (angle == 180) {
    xp <- 1 - hj
    yp <- vj
  } else if (angle == 270) {
    xp <- vj
    yp <- 1 - hj
  }else {
    xp <- hj
    yp <- vj
  }

  x <- x %||% xp
  y <- y %||% yp

  # The gp settings can override element_gp
  gp <- gpar(fontsize = size, col = colour,
    fontfamily = family, fontface = face,
    lineheight = lineheight)
  element_gp <- gpar(fontsize = element$size, col = element$colour,
    fontfamily = element$family, fontface = element$face,
    lineheight = element$lineheight)

  textGrob(
    label, x, y, hjust = hj, vjust = vj,
    default.units = default.units,
    gp = modifyList(element_gp, gp),
    rot = angle, ...
  )
}


#' @S3method element_grob element_line
element_grob.element_line <- function(element, x = 0:1, y = 0:1,
  colour = NULL, size = NULL, linetype = NULL, lineend = NULL,
  default.units = "npc", id.lengths = NULL, ...) {

  # The gp settings can override element_gp
  gp <- gpar(lwd=len0_null(size * .pt), col=colour, lty=linetype, lineend = lineend)
  element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour,
    lty = element$linetype, lineend = element$lineend)

  polylineGrob(
    x, y, default.units = default.units,
    gp = modifyList(element_gp, gp),
    id.lengths = id.lengths, ...
  )
}



# Define an element's class and what other elements it inherits from
#
# @param class The name of class (like "element_line", "element_text",
#  or the reserved "character", which means a character vector (not
#  "character" class)
# @param inherit A vector of strings, naming the elements that this
#  element inherits from.
el_def <- function(class = NULL, inherit = NULL, description = NULL) {
  list(class = class, inherit = inherit, description = description)
}


# This data structure represents the theme elements and the inheritance
# among them.
.element_tree <- list(
  line                = el_def("element_line"),
  rect                = el_def("element_rect"),
  text                = el_def("element_text"),
  title               = el_def("element_text", "text"),
  axis.line           = el_def("element_line", "line"),
  axis.text           = el_def("element_text", "text"),
  axis.title          = el_def("element_text", "title"),
  axis.ticks          = el_def("element_line", "line"),
  legend.key.size     = el_def("unit"),
  panel.grid          = el_def("element_line", "line"),
  panel.grid.major    = el_def("element_line", "panel.grid"),
  panel.grid.minor    = el_def("element_line", "panel.grid"),
  strip.text          = el_def("element_text", "text"),
  strip.title         = el_def("element_text", "title"),

  axis.line.x         = el_def("element_line", "axis.line"),
  axis.line.y         = el_def("element_line", "axis.line"),
  axis.text.x         = el_def("element_text", "axis.text"),
  axis.text.y         = el_def("element_text", "axis.text"),
  axis.ticks.length   = el_def("unit"),
  axis.ticks.x        = el_def("element_line", "axis.ticks"),
  axis.ticks.y        = el_def("element_line", "axis.ticks"),
  axis.title.x        = el_def("element_text", "axis.title"),
  axis.title.y        = el_def("element_text", "axis.title"),
  axis.ticks.margin   = el_def("unit"),

  legend.background   = el_def("element_rect", "rect"),
  legend.margin       = el_def("unit"),
  legend.key          = el_def("element_rect", "rect"),
  legend.key.height   = el_def("unit", "legend.key.size"),
  legend.key.width    = el_def("unit", "legend.key.size"),
  legend.text         = el_def("element_text", "text"),
  legend.text.align   = el_def("character"),
  legend.title        = el_def("element_text", "title"),
  legend.title.align  = el_def("character"),
  legend.position     = el_def("character"),  # Need to also accept numbers
  legend.direction    = el_def("character"),
  legend.justification = el_def("character"),
  legend.box          = el_def("character"),
  legend.box.just     = el_def("character"),

  panel.background    = el_def("element_rect", "rect"),
  panel.border        = el_def("element_rect", "rect"),
  panel.margin        = el_def("unit"),
  panel.grid.major.x  = el_def("element_line", "panel.grid.major"),
  panel.grid.major.y  = el_def("element_line", "panel.grid.major"),
  panel.grid.minor.x  = el_def("element_line", "panel.grid.minor"),
  panel.grid.minor.y  = el_def("element_line", "panel.grid.minor"),

  strip.background    = el_def("element_rect", "rect"),
  strip.text.x        = el_def("element_text", "strip.text"),
  strip.text.y        = el_def("element_text", "strip.text"),
  strip.title.background = el_def("element_rect", "rect"),
  strip.title.x       = el_def("element_text", "strip.title"),
  strip.title.y       = el_def("element_text", "strip.title"),

  plot.background     = el_def("element_rect", "rect"),
  plot.title          = el_def("element_text", "title"),
  plot.margin         = el_def("unit"),

  aspect.ratio        = el_def("character")
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
validate_element <- function(el, elname) {
  eldef <- .element_tree[[elname]]

  if (is.null(eldef)) {
    stop('"', elname, '" is not a valid theme element name.')
  }

  # NULL values for elements are OK
  if (is.null(el)) return()

  if (eldef$class == "character") {
    # Need to be a bit looser here since sometimes it's a string like "top"
    # but sometimes its a vector like c(0,0)
    if (!is.character(el) && !is.numeric(el))
      stop("Element ", elname, " must be a string or numeric vector.")

  } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
      stop("Element ", elname, " must be a ", eldef$class, " object.")
  }
  invisible()
}
