# Define an element's class and what other elements it inherits from
#
# @param class The name of class (like "element_line", "element_text",
#  or the reserved "character", which means a character vector (not
#  "character" class)
# @param inherits A vector of strings, naming the elements that this
#  element inherits from.
el_def <- function(class = NULL, inherits = NULL, description = NULL) {
  list(class = class, inherits = inherits, description = description)
}


# This data structure represents the theme elements and the inheritance
# among them.
element_tree <- list(
  line                = el_def("element_line"),
  rect                = el_def("element_rect"),
  segment             = el_def("element_segment"),
  text                = el_def("element_text"),
  axis.text           = el_def("element_text", "text"),
  axis.title          = el_def("element_text", "text"),
  axis.ticks          = el_def("element_segment", "segment"),
  axis.ticks.length   = el_def("unit"),
  legend.key.size     = el_def("unit"),
  panel.grid          = el_def("element_line", "line"),
  panel.grid.major    = el_def("element_line", "panel.grid"),
  panel.grid.minor    = el_def("element_line", "panel.grid"),
  strip.text          = el_def("element_text", "text"),

  axis.line           = el_def("element_line", "line"),
  axis.text.x         = el_def("element_text", "axis.text"),
  axis.text.y         = el_def("element_text", "axis.text"),
  # x and y versions of these are new
  axis.ticks.x        = el_def("element_line", "axis.ticks"),
  axis.ticks.y        = el_def("element_line", "axis.ticks"),
  axis.title.x        = el_def("element_text", "axis.title"),
  axis.title.y        = el_def("element_text", "axis.title"),
  # x and y versions of these are new
  axis.ticks.length.x = el_def("unit", "axis.ticks.length"),
  axis.ticks.length.y = el_def("unit", "axis.ticks.length"),
  axis.ticks.margin   = el_def("unit"),

  legend.background   = el_def("element_rect", "rect"),
  legend.margin       = el_def("unit"),
  legend.key          = el_def("element_rect", "rect"),
  legend.key.height   = el_def("unit", "legend.key.size"),
  legend.key.width    = el_def("unit", "legend.key.size"),
  legend.text         = el_def("element_text", "text"),
  legend.text.align   = el_def("character"),
  legend.title        = el_def("element_text", "text"),
  legend.title.align  = el_def("character"),
  legend.position     = el_def("character"),  # Need to also accept numbers
  legend.direction    = el_def("character"),
  legend.justification = el_def("character"),
  legend.box          = el_def("character"),

  panel.background    = el_def("element_rect", "rect"),
  panel.border        = el_def("element_rect", "rect"),
  panel.margin        = el_def("unit"),
  # x and y versions of these are new
  panel.grid.major.x  = el_def("element_line", "panel.grid.major"),
  panel.grid.major.y  = el_def("element_line", "panel.grid.major"),
  # x and y versions of these are new
  panel.grid.minor.x  = el_def("element_line", "panel.grid.minor"),
  panel.grid.minor.y  = el_def("element_line", "panel.grid.minor"),

  strip.background    = el_def("element_rect", "rect"),
  strip.text.x        = el_def("element_text", "strip.text"),
  strip.text.y        = el_def("element_text", "strip.text"),

  plot.background     = el_def("element_rect", "rect"),
  plot.title          = el_def("element_text", "text"),
  plot.margin         = el_def("unit"),
  title               = el_def("character")
)


#' Get, set and update themes.
#' 
#' Use \code{theme_update} to modify a small number of elements of the current
#' theme or use \code{theme_set} to completely override it.
#' 
#' @param ... named list of theme settings
#' @export
#' @examples
#' qplot(mpg, wt, data = mtcars)
#' old <- theme_set(theme_bw())
#' qplot(mpg, wt, data = mtcars)
#' theme_set(old)
#' qplot(mpg, wt, data = mtcars)
#'
#' old <- theme_update(panel.background = theme_rect(colour = "pink"))
#' qplot(mpg, wt, data = mtcars)
#' theme_set(old)
#' theme_get()
#' 
#' qplot(mpg, wt, data=mtcars, colour=mpg) + 
#'   opts(legend.position=c(0.95, 0.95), legend.justification = c(1, 1))
#' last_plot() + 
#'  opts(legend.background = theme_rect(fill = "white", col="white", size =3))
theme_update <- function(...) {
  elements <- list(...)
  if (length(args) == 1 && is.list(elements[[1]])) {
    elements <- elements[[1]]
  }
  theme <- defaults(elements, theme_get())
  class(theme) <- c("options")
  
  theme_set(theme)  
}

.theme <- (function() {
  theme <- theme_gray()

  list(
    get = function() theme,
    set = function(new) {
      missing <- setdiff(names(theme_gray()), names(new))
      if (length(missing) > 0) {
        warning("New theme missing the following elements: ", 
          paste(missing, collapse = ", "), call. = FALSE)
      }
      
      old <- theme
      theme <<- new
      invisible(old)
    }
  )
})()

#' @rdname theme_update
#' @export
theme_get <- .theme$get  
#' @rdname theme_update
#' @param new new theme (a list of theme elements)
#' @export
theme_set <- .theme$set

#' Set options/theme elements for a single plot
#' 
#' 
#' Use this function if you want to modify a few theme settings for 
#' a single plot. 
#' 
#' @section Theme elements:
#' The individual theme elements are:
#' 
#' \tabular{ll}{
#'   axis.line     \tab  line along axis \cr
#'   axis.title.x   \tab   x axis label \cr
#'   axis.title.y    \tab  y axis label \cr
#'   axis.ticks     \tab  axis tick marks \cr
#'   axis.ticks.length \tab  tick mark length \cr 
#'   axis.ticks.margin   \tab   tick mark margin spacing \cr
#'   axis.text.x  \tab   horizontal tick labels \cr
#'   axis.text.y  \tab   vertical tick labels \cr
#'   legend.background   \tab   background of legend \cr
#'   legend.margin \tab   extra space added around legend (both width or
#'      height depending on orientation of legend) \cr
#'   legend.key   \tab    background underneath legend keys \cr
#'   legend.key.size   \tab     key background size \cr
#'   legend.key.height \tab     key background height \cr
#'   legend.key.width  \tab     key background width \cr
#'   legend.text       \tab     legend labels \cr
#'   legend.text.align \tab     alignment of legend labels \cr
#'   legend.title      \tab     legend name \cr
#'   legend.title.align  \tab   alignment of legend title \cr
#'   legend.position    \tab    A string or numeric vector  specifying the
#'      position of guides (legends).  Possible values are: "left", "right",
#'     "bottom", "top", and two-element numeric vector. \cr
#'   legend.justification \tab  alignment of legend \cr
#'   legend.direction     \tab  horizontal or vertical \cr
#'   legend.box   \tab    A string specifying the direction of multiple
#'     guides. Possible string values are: "horizontal" and "vertical". \cr
#'   panel.background   \tab    background of panel \cr
#'   panel.border       \tab    border around panel \cr
#'   panel.margin       \tab    margin around facet panels \cr
#'   panel.grid.major   \tab    major grid lines \cr 
#'   panel.grid.minor   \tab    minor grid lines \cr
#'   plot.background    \tab    background of the entire plot \cr
#'   plot.title         \tab    plot title (text appearance) \cr
#'   plot.margin        \tab    plot margins \cr
#'   strip.background   \tab    background of facet labels \cr
#'   strip.text.x       \tab    text for horizontal strips \cr
#'   strip.text.y       \tab    text for vertical strips \cr
#'   title              \tab    A string containing the title of the plot \cr
#'  }
#'
#' @param ... a list of element name, element pairings that modify the
#'   existing theme.
#' @export
#' @examples
#' \donttest{
#' p <- qplot(mpg, wt, data = mtcars)
#' p 
#' p + opts(panel_background = theme_rect(colour = "pink"))
#' p + theme_bw()
#'
#' # Scatter plot of gas mileage by vehicle weight
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + geom_abline(intercept = 37, slope = -5) 
#' # Calculate correlation coefficient
#' with(mtcars, cor(wt, mpg, use = "everything", method = "pearson"))
#' #annotate the plot
#' p + geom_abline(intercept = 37, slope = -5) + 
#' geom_text(data = data.frame(), aes(4.5, 30, label = "Pearson-R = -.87"))
#'
#' # Change the axis labels
#' # Original plot
#' p
#' p + xlab("Vehicle Weight") + ylab("Miles per Gallon")
#' # Or
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#' 
#' # Add a title to the plot
#' p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
#' # Change title appearance
#' p <- p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
#' p + opts(plot.title = theme_text(size = 20))
#' p + opts(plot.title = theme_text(size = 20, colour = "Blue"))
#'
#' # Changing plot look with themes
#' DF <- data.frame(x = rnorm(400))
#' m <- ggplot(DF, aes(x = x)) + geom_histogram()
#' #default is theme_grey()
#' m 
#' # Compare with
#' m + theme_bw()
#' 
#' # Manipulate Axis Attributes
#' library(grid) # for unit
#' m + opts(axis.line = theme_segment())
#' m + opts(axis.line = theme_segment(colour = "red", linetype = "dotted"))
#' m + opts(axis.text.x = theme_text(colour = "blue"))
#' m + opts(axis.text.y = theme_blank())
#' m + opts(axis.ticks = theme_segment(size = 2))
#' m + opts(axis.title.y = theme_text(size = 20, angle = 90))
#' m + opts(axis.title.x = theme_blank())
#' m + opts(axis.ticks.length = unit(.85, "cm"))
#'
#' # Legend Attributes
#' z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
#' z
#' z + opts(legend.position = "none")
#' z + opts(legend.position = "bottom")
#' # Or use relative coordinates between 0 and 1
#' z + opts(legend.position = c(.5, .5))
#  # Add a border to the whole legend
#' z + opts(legend.background = theme_rect())
#' # Legend margin controls extra space around outside of legend:
#' z + opts(legend.background = theme_rect(), legend.margin = unit(1, "cm"))
#' z + opts(legend.background = theme_rect(), legend.margin = unit(0, "cm"))
#' # Or to just the keys
#' z + opts(legend.key = theme_rect())
#' z + opts(legend.key = theme_rect(fill = "yellow"))
#' z + opts(legend.key.size = unit(2.5, "cm"))
#' z + opts(legend.text = theme_text(size = 20, colour = "red", angle = 45))
#' z + opts(legend.title = theme_text(face = "italic"))
#'
#' # To change the title of the legend use the name argument
#' # in one of the scale options
#' z + scale_colour_brewer(name = "My Legend")
#' z + scale_colour_grey(name = "Number of \nCylinders")
#'
#' # Panel and Plot Attributes
#' z + opts(panel.background = theme_rect())
#' z + opts(panel.background = theme_rect(fill = "black"))
#' z + opts(panel.border = theme_rect(linetype = "dashed"))
#' z + opts(panel.grid.major = theme_line(colour = "blue"))
#' z + opts(panel.grid.minor = theme_line(colour = "red", linetype = "dotted"))
#' z + opts(panel.grid.major = theme_line(size = 2))
#' z + opts(plot.background = theme_rect())
#' z + opts(plot.background = theme_rect(fill = "grey"))
#'
#' # Faceting Attributes
#' set.seed(4940)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' k <- ggplot(dsmall, aes(carat, ..density..)) +  geom_histogram(binwidth = 0.2) +
#' facet_grid(. ~ cut)
#' k + opts(strip.background = theme_rect(colour = "purple", fill = "pink", size = 3, linetype = "dashed"))
#' k + opts(strip.text.x = theme_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5))
#' k + opts(panel.margin = unit(5, "lines"))
#' k + opts(panel.margin = unit(0, "lines"))
#' }
opts <- function(...) {
  structure(list(...), class="options")
}

# Render a theme element
theme_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, element_tree, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  ggname(ps(element, name, sep = "."), element_grob(el, ...))
}

element_grob <- function(element, ...)
  UseMethod("element_grob")

# TODO: Get rid of ... and id.lengths
#' @S3method element_grob element_blank
element_grob.element_blank <- function(element, ...)  zeroGrob()

#' @S3method element_grob element_rect
element_grob.element_rect <- function(element, x = 0.5, y = 0.5,
  width = 1, height = 1, ...) {

  element_gp <- gpar(lwd = element$size * .pt, col = element$colour,
    fill = element$fill, lty = element$linetype)

  rectGrob(x, y, width, height, gp = element_gp)
}


#' @S3method element_grob element_text
element_grob.element_text <- function(element,
  label = "", x = 0.5, y = 0.5, default.units = "npc") {

  vj <- element$vjust
  hj <- element$hjust
  angle <- element$angle %% 360
  
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

  element_gp <- gpar(lwd = element$size * .pt, col = element$colour,
    fill = element$fill, lty = element$linetype)

  textGrob(
    label, x, y, hjust = hj, vjust = vj,
    default.units = default.units,
    gp = element_gp,
    rot = angle
  )
}


#' @S3method element_grob element_segment
element_grob.element_segment <- function(element, x0 = 0, y0 = 0,
  x1 = 1, y1 = 1) {

  element_gp <- gpar(lwd = element$size * .pt, col = element$colour,
    lty = element$linetype)

  segmentsGrob(
    x0, y0, x1, y1, default.units = "npc",
    gp = element_gp
  )
}


# TODO: remove this ... and get rid of id.lengths
#' @S3method element_grob element_line
element_grob.element_line <- function(element, x = 0:1, y = 0:1,
  default.units = "npc", id.lengths = NULL) {

  element_gp <- gpar(lwd = element$size * .pt, col = element$colour,
    lty = element$linetype)

  polylineGrob(
    x, y, default.units = default.units,
    gp = element_gp,
    id.lengths = id.lengths
  )
}


#' @S3method print theme
print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$options, theme_get())
}

##' Update contents of a theme
##'
##' @title Update theme param
##' @param name name of a theme element
##' @param ... Pairs of name and value of theme parameters.
##' @return Updated theme element
##' @export
##' @examples
##' x <- theme_text(size = 15)
##' update_element(x, colour = "red")
##' # Partial matching works
##' update_element(x, col = "red")
##' # So does positional
##' update_element(x, "Times New Roman")
##' # And it throws an error if you use an argument that doesn't exist
##' \donttest{update_element(x, noargument = 12)}
##' # Or multiple arguments with the same name
##' \donttest{update_element(x, size = 12, size = 15)}
##' 
##' # Will look up element if given name
##' update_element("axis.text.x", colour = 20)
##' # Throws error if incorrectly named
##' \donttest{update_element("axis.text", colour = 20)}
update_element <- function(name, ...) {
 if (is.character(name)) {
   ele <- theme_get()[[name]]
   if (is.null(ele)) {
     stop("Could not find theme element ", name, call. = FALSE)
   }
 } else {
   ele <- name
 }

 call <- attr(ele, "call")
 stopifnot(!is.null(call))

 # Partial matching of named ... args with full names
 f <- eval(call[[1]])
 new_args <- match.call()
 new_args$name <- NULL
 new_args <- as.list(match.call(f, new_args)[-1])

 # Combine old call with new args
 old <- as.list(call)

 # evaluate old args in its env
 evaled_old_args <- llply(names(old[-1]), get, environment(ele))
 names(evaled_old_args) <- names(old[-1])
 # replace premise with evaluated vars
 old <- modifyList(old, evaled_old_args)

 eval(as.call(modifyList(old, new_args)))
}


# Calculate the element properties, by inheriting properties from its parents
#
# @param element The name of the theme element to calculate
# @param tree An element inheritance tree
# @theme theme A theme object (like theme_grey())
calc_element <- function(element, tree, theme) {
  # If this is element_blank, don't inherit anything from parents
  if (inherits(theme[[element]], "element_blank"))
    return(theme[[element]])

  # Get the names of parents from the inheritance tree
  pnames <- tree[[element]]$inherits

  # If no parents, just return this element
  if (is.null(pnames))
    return(theme[[element]])

  # Calculate the parent objects' inheritance
  parents <- lapply(pnames, calc_element, tree, theme)

  # Combine the propertiesl of this element with all parents
  Reduce(combine_elements, parents, theme[[element]])
}


# Combine the properties of two elements
#
# @param e1 An element object
# @param e2 An element object which e1 inherits from
combine_elements <- function(e1, e2) {
  # TODO Check that classes align

  # If e2 is NULL, nothing to inherit
  if (is.null(e2))  return(e1)

  # If e1 is NULL, inherit everything from e2
  if (is.null(e1))  return(e2)

  # If e1 has any NULL properties, inherit them from e2
  n <- sapply(e1[names(e2)], is.null)
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}
