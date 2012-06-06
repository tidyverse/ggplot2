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
#' old <- theme_update(panel.background = element_rect(colour = "pink"))
#' qplot(mpg, wt, data = mtcars)
#' theme_set(old)
#' theme_get()
#' 
#' qplot(mpg, wt, data=mtcars, colour=mpg) + 
#'   theme(legend.position=c(0.95, 0.95), legend.justification = c(1, 1))
#' last_plot() + 
#'  theme(legend.background = element_rect(fill = "white", col="white", size = 3))
theme_update <- function(...) {
  # Make a call to theme, then add to theme
  theme_set(theme_get() + do.call(theme, list(...)))
}

#' Reports whether x is a theme object
#' @export
is.theme <- function(x) inherits(x, "theme")

#' @S3method print theme
print.theme <- function(x, ...) str(x)

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
#' p + theme(panel_background = element_rect(colour = "pink"))
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
#' # Change title appearance
#' p <- p + labs(title = "Vehicle Weight-Gas Mileage Relationship")
#' p + theme(plot.title = element_text(size = 20))
#' p + theme(plot.title = element_text(size = 20, colour = "Blue"))
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
#' m + theme(axis.line = element_segment())
#' m + theme(axis.line = element_segment(colour = "red", linetype = "dotted"))
#' m + theme(axis.text.x = element_text(colour = "blue"))
#' m + theme(axis.text.y = element_blank())
#' m + theme(axis.ticks = element_segment(size = 2))
#' m + theme(axis.title.y = element_text(size = 20, angle = 90))
#' m + theme(axis.title.x = element_blank())
#' m + theme(axis.ticks.length = unit(.85, "cm"))
#'
#' # Legend Attributes
#' z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
#' z
#' z + theme(legend.position = "none")
#' z + theme(legend.position = "bottom")
#' # Or use relative coordinates between 0 and 1
#' z + theme(legend.position = c(.5, .5))
#  # Add a border to the whole legend
#' z + theme(legend.background = element_rect())
#' # Legend margin controls extra space around outside of legend:
#' z + theme(legend.background = element_rect(), legend.margin = unit(1, "cm"))
#' z + theme(legend.background = element_rect(), legend.margin = unit(0, "cm"))
#' # Or to just the keys
#' z + theme(legend.key = element_rect())
#' z + theme(legend.key = element_rect(fill = "yellow"))
#' z + theme(legend.key.size = unit(2.5, "cm"))
#' z + theme(legend.text = element_text(size = 20, colour = "red", angle = 45))
#' z + theme(legend.title = element_text(face = "italic"))
#'
#' # To change the title of the legend use the name argument
#' # in one of the scale options
#' z + scale_colour_brewer(name = "My Legend")
#' z + scale_colour_grey(name = "Number of \nCylinders")
#'
#' # Panel and Plot Attributes
#' z + theme(panel.background = element_rect())
#' z + theme(panel.background = element_rect(fill = "black"))
#' z + theme(panel.border = element_rect(linetype = "dashed"))
#' z + theme(panel.grid.major = element_line(colour = "blue"))
#' z + theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"))
#' z + theme(panel.grid.major = element_line(size = 2))
#' z + theme(plot.background = element_rect())
#' z + theme(plot.background = element_rect(fill = "grey"))
#'
#' # Faceting Attributes
#' set.seed(4940)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' k <- ggplot(dsmall, aes(carat, ..density..)) +  geom_histogram(binwidth = 0.2) +
#' facet_grid(. ~ cut)
#' k + theme(strip.background = element_rect(colour = "purple", fill = "pink", size = 3, linetype = "dashed"))
#' k + theme(strip.text.x = element_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5))
#' k + theme(panel.margin = unit(5, "lines"))
#' k + theme(panel.margin = unit(0, "lines"))
#' }
theme <- function(...) {
  structure(list(...), class="theme")
}


#' Build a theme (or partial theme) from theme elements
#'
#' \code{opts} is deprecated. See the \code{\link{theme}} function.
#'
#' @export
opts <- function(...) {
  .Deprecated(new = "theme")

  # Add check for deprecated elements
  extra <- NULL
  elements <- list(...)
  if (!is.null(elements[["title"]])) {
    # This is kind of a hack, but fortunately it will be removed in future versions
    warning('Setting the plot title with opts(title="...") is deprecated.',
      ' Use labs(title="...") or ggtitle("...") instead.')

    title <- elements$title
    elements$title <- NULL
    return(list(ggtitle(title), theme(elements)))
  }

  theme(elements)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$options, theme_get())
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


#' @rdname theme-add
#' @export
"%+replace%" <- function(e1, e2) {
  if (!inherits(e1, "theme") || !inherits(e2, "theme")) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }

  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}


#' Modify properties of an element in a theme object
#'
#' @seealso +.theme
#'
add_theme <- function(t1, t2) {
  if (!inherits(t2, "theme")) {
    stop("Don't know how to add ", orig_args(t2), " to an options object",
      call. = FALSE)
  }

  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]

    if (is.null(x)) {
      # If x is NULL, then just assign it y
      x <- y
    } else {
      # If x is not NULL, then copy over the non-NULL properties from y
      # Get logical vector of non-NULL properties in y
      idx <- !vapply(y, is.null, logical(1))
      # Get the names of TRUE items
      idx <- names(idx[idx])

      # Update non-NULL items
      x[idx] <- y[idx]
    }

    # Assign it back to t1
    t1[[item]] <- x
  }

  t1
}


##' Update contents of a theme
##'
##' @title Update theme param
##' @param name name of a theme element
##' @param ... Pairs of name and value of theme parameters.
##' @return Updated theme element
##' @export
##' @examples
##' x <- element_text(size = 15)
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


#' Calculate the element properties, by inheriting properties from its parents
#'
#' @param element The name of the theme element to calculate
#' @param theme A theme object (like theme_grey())
#' @export
calc_element <- function(element, theme) {
  # If this is element_blank, don't inherit anything from parents
  if (inherits(theme[[element]], "element_blank"))
    return(theme[[element]])

  # If the element is defined (and not just inherited), check that
  # it is of the class specified in .element_tree
  if (!is.null(theme[[element]]) &&
      !inherits(theme[[element]], .element_tree[[element]]$class)) {
    stop(element, " should have class ", .element_tree[[element]]$class)
  }

  # Get the names of parents from the inheritance tree
  pnames <- .element_tree[[element]]$inherits

  # If no parents, just return this element
  if (is.null(pnames)) {
    # First check that there all the properties of this element are non-NULL
    nullprops <- vapply(theme[[element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("Theme element ", element, " has NULL property: ",
        paste(names(nullprops)[nullprops], collapse = ", "))
    }

    return(theme[[element]])
  }

  # Calculate the parent objects' inheritance
  parents <- lapply(pnames, calc_element, theme)

  # If this element is not NULL, then
  # don't try to inherit from parents that are element_blank
  if (!is.null(theme[[element]]))
    parents <- parents[!vapply(parents, inherits, logical(1), "element_blank")]

  # Combine the properties of this element with all parents
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
  n <- vapply(e1[names(e2)], is.null, logical(1))
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}
