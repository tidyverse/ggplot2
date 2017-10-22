#' Theme elements
#'
#' @description
#' In conjunction with the \link{theme} system, the `element_` functions
#' specify the display of how non-data components of the plot are a drawn.
#'
#'   - `element_blank`: draws nothing, and assigns no space.
#'   - `element_rect`: borders and backgrounds.
#'   - `element_line`: lines.
#'   - `element_text`: text.
#'
#' `rel()` is used to specify sizes relative to the parent,
#' `margins()` is used to specify the margins of elements.
#'
#' @param fill Fill colour.
#' @param colour,color Line/border colour. Color is an alias for colour.
#' @param size Line/border size in mm; text size in pts.
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
#'     size = 1
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
element_rect <- function(fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, color = NULL, inherit.blank = FALSE) {

  if (!is.null(color))  colour <- color
  structure(
    list(fill = fill, colour = colour, size = size, linetype = linetype,
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
element_line <- function(colour = NULL, size = NULL, linetype = NULL,
  lineend = NULL, color = NULL, arrow = NULL, inherit.blank = FALSE) {

  if (!is.null(color))  colour <- color
  if (is.null(arrow)) arrow <- FALSE
  structure(
    list(colour = colour, size = size, linetype = linetype, lineend = lineend,
      arrow = arrow, inherit.blank = inherit.blank),
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

# Given a theme object and element name, return a grob for the element
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  grob <- element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}


# Returns NULL if x is length 0
len0_null <- function(x) {
  if (length(x) == 0)  NULL
  else                 x
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
  fill = NULL, colour = NULL, size = NULL, linetype = NULL, ...) {

  # The gp settings can override element_gp
  gp <- gpar(lwd = len0_null(size * .pt), col = colour, fill = fill, lty = linetype)
  element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour,
    fill = element$fill, lty = element$linetype)

  rectGrob(x, y, width, height, gp = utils::modifyList(element_gp, gp), ...)
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

  angle <- angle %||% element$angle
  if (is.null(angle)) {
    stop("Text element requires non-NULL value for 'angle'.")
  }

  # The gp settings can override element_gp
  gp <- gpar(fontsize = size, col = colour,
    fontfamily = family, fontface = face,
    lineheight = lineheight)
  element_gp <- gpar(fontsize = element$size, col = element$colour,
    fontfamily = element$family, fontface = element$face,
    lineheight = element$lineheight)

  titleGrob(label, x, y, hjust = hj, vjust = vj, angle = angle,
    gp = utils::modifyList(element_gp, gp), margin = margin,
    margin_x = margin_x, margin_y = margin_y, debug = element$debug)
}



#' @export
element_grob.element_line <- function(element, x = 0:1, y = 0:1,
  colour = NULL, size = NULL, linetype = NULL, lineend = NULL,
  default.units = "npc", id.lengths = NULL, ...) {

  # The gp settings can override element_gp
  gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype, lineend = lineend)
  element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour,
    lty = element$linetype, lineend = element$lineend)
  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  } else {
    element$arrow
  }
  polylineGrob(
    x, y, default.units = default.units,
    gp = utils::modifyList(element_gp, gp),
    id.lengths = id.lengths, arrow = arrow, ...
  )
}

#' Define an element's class and what other elements it inherits from
#'
#' @param class The name of class (like "element_line", "element_text",
#'  or the reserved "character", which means a character vector (not
#'  "character" class)
#' @param inherit A vector of strings, naming the elements that this
#'  element inherits from.
#' @param description The optional description of the particular element.
#' @export
el_def <- function(class = NULL, inherit = NULL, description = NULL) {
  structure(
    list(class = class, inherit = inherit, description = description),
    class=c('el_def','list')
  )
}

#' R6 Element Tree Definition
#'
#' This is an R6 Class definition, to hold the element tree structure.
#' Contains a reserved default set for use by ggplot2, and contains the methods
#' that permit the user to add additional definitions, provided they do not conflict
#' with the reserved structure. The Final structure and indexing functions, ie, those that
#' permit equal substitution with previous ggplot2 methods, is produced via combining
#' the default and user-defined element trees.
#' @author Nicholas Hamilton, UNSW Sydney
#' @importFrom R6 R6Class
R6ElementTree = R6Class('element_tree',
  public = list(
    initialize = function(){
      self$reset(FALSE)
    },
    reset = function(verbose=TRUE){
      if(verbose && length(private$varElementsUser) > 0)
        warning("Resetting to default element tree, user elements have been discarded.",call.=FALSE)
      private$varLocked = TRUE
      private$varElementsFixed = private$varElementsFixedDefault
      private$varElementsUser  = list()
    },
    get = function(){
      self$get_elements()
    },
    add = function(k,v){
      if(!inherits(v,'el_def'))
        stop("value must inherit from 'el_def'",call.=FALSE)
      if(!inherits(k,'character') || length(k) != 1)
        stop("key must be 'character' of length 1",call.=FALSE)
      if(self$is_locked()){
        if(k %in% names(private$varElementsFixed))
          stop(sprintf("The key '%s' is a protected name, reserved for base ggplot2.",k),call.=FALSE)
        if(k %in% names(private$varElementsUser))
          warning(sprintf("The key '%s' exists in the user list, overwriting.",k),call.=FALSE)
        private$varElementsUser[[ k ]] = v
      }else{
        if(k %in% names(private$varElementsFixed))
          warning(sprintf("The key '%s' exists in the protected list, overwriting.",k),call.=FALSE)
        if(k %in% names(private$varElementsUser))
          private$varElementsUser[[ k ]] = NULL
        private$varElementsFixed[[ k ]] = v
      }
      invisible(self)
    },
    at = function(k){
      self$get()[[k]] %||% stop(sprintf("'%s' is not a valid theme element name.",as.character(k)),call.=FALSE)
    },

    add_element = function(k,v){
      self$add(k,v)
    },
    get_elements = function(){
      c(self$get_elements_fixed(),
        self$get_elements_user())
    },
    get_elements_fixed = function(){
      private$varElementsFixed
    },
    get_elements_user = function(){
      private$varElementsUser
    },
    get_private = function(){
      self$`.__enclos_env__`$private
    },
    is_locked = function(){
      private$varLocked
    },
    print = function(){
      print(self$get())
      invisible(self)
    },
    validate_element = function(el, elname){
      eldef <- self$at(elname)

      # NULL values for elements are OK
      if (is.null(el)) return()

      if (eldef$class == "character") {
        # Need to be a bit looser here since sometimes it's a string like "top"
        # but sometimes its a vector like c(0,0)
        if (!is.character(el) && !is.numeric(el))
          stop("Element ", elname, " must be a string or numeric vector.",call.=FALSE)
      } else if (eldef$class == "margin") {
        if (!is.unit(el) && length(el) == 4)
          stop("Element ", elname, " must be a unit vector of length 4.",call.=FALSE)
      } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
        stop("Element ", elname, " must be a ", eldef$class, " object.",call.=FALSE)
      }
      invisible()
    }
  ),
  private = list(
    lock = function(){
      private$varLocked = TRUE
      invisible(self)
    },
    unlock = function(){
      private$varLocked = FALSE
      invisible(self)
    },
    varElementsFixedDefault = list(
      line                  = el_def("element_line"),
      rect                  = el_def("element_rect"),
      text                  = el_def("element_text"),
      title                 = el_def("element_text", "text"),
      axis.line             = el_def("element_line", "line"),
      axis.text             = el_def("element_text", "text"),
      axis.title            = el_def("element_text", "title"),
      axis.ticks            = el_def("element_line", "line"),
      legend.key.size       = el_def("unit"),
      panel.grid            = el_def("element_line", "line"),
      panel.grid.major      = el_def("element_line", "panel.grid"),
      panel.grid.minor      = el_def("element_line", "panel.grid"),
      strip.text            = el_def("element_text", "text"),

      axis.line.x           = el_def("element_line", "axis.line"),
      axis.line.x.top       = el_def("element_line", "axis.line.x"),
      axis.line.x.bottom    = el_def("element_line", "axis.line.x"),
      axis.line.y           = el_def("element_line", "axis.line"),
      axis.line.y.left      = el_def("element_line", "axis.line.y"),
      axis.line.y.right     = el_def("element_line", "axis.line.y"),
      axis.text.x           = el_def("element_text", "axis.text"),
      axis.text.x.top       = el_def("element_text", "axis.text.x"),
      axis.text.x.bottom    = el_def("element_text", "axis.text.x"),
      axis.text.y           = el_def("element_text", "axis.text"),
      axis.text.y.left      = el_def("element_text", "axis.text.y"),
      axis.text.y.right     = el_def("element_text", "axis.text.y"),
      axis.ticks.length     = el_def("unit"),
      axis.ticks.x          = el_def("element_line", "axis.ticks"),
      axis.ticks.x.top      = el_def("element_line", "axis.ticks.x"),
      axis.ticks.x.bottom   = el_def("element_line", "axis.ticks.x"),
      axis.ticks.y          = el_def("element_line", "axis.ticks"),
      axis.ticks.y.left     = el_def("element_line", "axis.ticks.y"),
      axis.ticks.y.right    = el_def("element_line", "axis.ticks.y"),
      axis.title.x          = el_def("element_text", "axis.title"),
      axis.title.x.top      = el_def("element_text", "axis.title.x"),
      axis.title.x.bottom   = el_def("element_text", "axis.title.x"),
      axis.title.y          = el_def("element_text", "axis.title"),
      axis.title.y.left     = el_def("element_text", "axis.title.y"),
      axis.title.y.right    = el_def("element_text", "axis.title.y"),

      legend.background     = el_def("element_rect", "rect"),
      legend.margin         = el_def("margin"),
      legend.spacing        = el_def("unit"),
      legend.spacing.x      = el_def("unit", "legend.spacing"),
      legend.spacing.y      = el_def("unit", "legend.spacing"),
      legend.key            = el_def("element_rect", "rect"),
      legend.key.height     = el_def("unit", "legend.key.size"),
      legend.key.width      = el_def("unit", "legend.key.size"),
      legend.text           = el_def("element_text", "text"),
      legend.text.align     = el_def("character"),
      legend.title          = el_def("element_text", "title"),
      legend.title.align    = el_def("character"),
      legend.position       = el_def("character"),  # Need to also accept numbers
      legend.direction      = el_def("character"),
      legend.justification  = el_def("character"),
      legend.box            = el_def("character"),
      legend.box.just       = el_def("character"),
      legend.box.margin     = el_def("margin"),
      legend.box.background = el_def("element_rect", "rect"),
      legend.box.spacing    = el_def("unit"),

      panel.background      = el_def("element_rect", "rect"),
      panel.border          = el_def("element_rect", "rect"),
      panel.spacing         = el_def("unit"),
      panel.spacing.x       = el_def("unit", "panel.spacing"),
      panel.spacing.y       = el_def("unit", "panel.spacing"),
      panel.grid.major.x    = el_def("element_line", "panel.grid.major"),
      panel.grid.major.y    = el_def("element_line", "panel.grid.major"),
      panel.grid.minor.x    = el_def("element_line", "panel.grid.minor"),
      panel.grid.minor.y    = el_def("element_line", "panel.grid.minor"),
      panel.ontop           = el_def("logical"),

      strip.background      = el_def("element_rect", "rect"),
      strip.text.x          = el_def("element_text", "strip.text"),
      strip.text.y          = el_def("element_text", "strip.text"),
      strip.placement       = el_def("character"),
      strip.placement.x     = el_def("character", "strip.placement"),
      strip.placement.y     = el_def("character", "strip.placement"),
      strip.switch.pad.grid = el_def("unit"),
      strip.switch.pad.wrap = el_def("unit"),

      plot.background       = el_def("element_rect", "rect"),
      plot.title            = el_def("element_text", "title"),
      plot.subtitle         = el_def("element_text", "title"),
      plot.caption          = el_def("element_text", "title"),
      plot.margin           = el_def("margin"),

      aspect.ratio          = el_def("character")
    ),
    varElementsFixed = list(
    ),
    varElementsUser = list(
    ),
    varLocked = TRUE
  )
)
`[[.element_tree` = function(e,i){ e$get_elements()[[i]] }
`[.element_tree`  = function(e,i){ e$get_elements()[i] }

# This data structure represents the theme elements and the inheritance
# among them.
.element_tree <- R6ElementTree$new()

#' Add User Element to Element Tree
#'
#' \code{add_element} is a function that adds a user defined element, to the element tree,
#' if it does not conflict with the default element tree required for the
#' functioning of base \code{ggplot2}.
#' @param k the character index (key) for the element,
#' must be not present in the fixed (reserved) list of elements for the base ggplot2 package,
#' and an error will be thrown if either k is not a character of length 1, or k is protected.
#' @param v the \code{\link{el_def}} object (value) that the user wishes to create.
#' @examples
#' #Add a single element
#' add_element('mycustomtitle',el_def("element_text", "title"))
#'
#' #Without adding the element above, the following would otherwise throw an error,
#' #reporting 'mycustomtitle' is not a valid theme element
#' mytheme = theme(mycustomtitle = element_text())
#'
#' #Try to add a reserved name, will throw an error, protecting functionality of base ggplot2
#' \dontrun{add_element('plot.title',el_def('element_text','title'))}
#'
#' @author Nicholas Hamilton, UNSW Sydney
#' @export
#' @rdname element_tree
add_element = function(k,v){ .element_tree$add_element(k,v) }

#' Get Complete List of Fixed and User-defined Elements
#'
#' \code{get_elements} is a function that returns the complete list of elements,
#' forming the fixed and user defined (if present) elements comprising the element tree.
#' @export
#' @rdname element_tree
get_elements = function(){ .element_tree$get() }

#' Check that an element object has the proper class
#'
#' Given an element object and the name of the element, this function
#' checks it against the element inheritance tree to make sure the
#' element is of the correct class
#'
#' It throws error if invalid, and returns invisible() if valid.
#'
#' @param el an element
#' @param elname the name of the element
validate_element <- function(el, elname) {
  .element_tree$validate_element(el,elname)
}
