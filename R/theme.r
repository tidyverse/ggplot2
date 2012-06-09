#' Get, set and update themes.
#' 
#' Use \code{theme_update} to modify a small number of elements of the current
#' theme or use \code{theme_set} to completely override it.
#' 
#' @param ... named list of theme settings
#' @seealso \code{\link{\%+replace\%}} and \code{\link{+.theme}}
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
  theme_set(theme_get() %+replace% do.call(theme, list(...)))
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
theme <- function(..., complete = FALSE) {
  elements <- list(...)

  # Check that all elements have the correct class (element_text, unit, etc)
  mapply(validate_element, elements, names(elements))

  structure(elements, class = "theme", complete = complete)
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

    return(list(ggtitle(title), do.call(theme, elements)))
  }

  do.call(theme, elements)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$theme, theme_get())
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
  if (!is.theme(e1) || !is.theme(e2)) {
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
add_theme <- function(t1, t2, t2name) {
  if (!is.theme(t2)) {
    stop("Don't know how to add ", t2name, " to a theme object",
      call. = FALSE)
  }

  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]

    if (is.null(x)) {
      # If x is NULL, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y)) {
      # If y is NULL, or a string or numeric vector, just replace x
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
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }

  # If either theme is complete, then the combined theme is complete
  attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
  t1
}


# Update a theme from a plot object
#
# This is called from add_ggplot.
#
# If newtheme is a *complete* theme, then it is meant to replace
# oldtheme; this function just returns newtheme.
#
# Otherwise, it adds elements from newtheme to oldtheme:
# If oldtheme doesn't already contain those elements,
# it searches the current default theme, grabs the elements with the
# same name as those from newtheme, and puts them in oldtheme. Then
# it adds elements from newtheme to oldtheme.
# This makes it possible to do things like:
#   qplot(1:3, 1:3) + theme(text = element_text(colour = 'red'))
# and have 'text' keep properties from the default theme. Otherwise
# you would have to set all the element properties, like family, size,
# etc.
#
# @param oldtheme an existing theme, usually from a plot object, like
#   plot$theme. This could be an empty list.
# @param newtheme a new theme object to add to the existing theme
update_theme <- function(oldtheme, newtheme) {
  # If the newtheme is a complete one, don't bother searching
  # the default theme -- just replace everything with newtheme
  if (attr(newtheme, "complete"))
    return(newtheme)

  # These are elements in newtheme that aren't already set in oldtheme.
  # They will be pulled from the default theme.
  newitems <- ! names(newtheme) %in% names(oldtheme)
  newitem_names <- names(newtheme)[newitems]
  oldtheme[newitem_names] <- theme_get()[newitem_names]

  # Update the theme elements with the things from newtheme
  # Turn the 'theme' list into a proper theme object first, and preserve
  # the 'complete' attribute. It's possible that oldtheme is an empty
  # list, and in that case, set complete to FALSE.
  oldtheme <- do.call(theme, c(oldtheme,
    complete = isTRUE(attr(oldtheme, "complete"))))

  oldtheme + newtheme
}


##' Update contents of a theme. (Deprecated)
##'
##' This function is deprecated. Use \code{\link{\%+replace\%}} or
##' \code{\link{+.theme}} instead.
##'
##' @title Update theme param
##' @param name name of a theme element
##' @param ... Pairs of name and value of theme parameters.
##' @return Updated theme element
##' @seealso \code{\link{\%+replace\%}} and \code{\link{+.theme}}
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
  .Deprecated(new = "+.theme")
 if (is.character(name)) {
   ele <- theme_get()[[name]]
   if (is.null(ele)) {
     stop("Could not find theme element ", name, call. = FALSE)
   }
 } else {
   ele <- name
 }

  stopifnot(inherits(ele, "element"))

  modifyList(ele, list(...))
}


#' Calculate the element properties, by inheriting properties from its parents
#'
#' @param element The name of the theme element to calculate
#' @param theme A theme object (like theme_grey())
#' @param verbose If TRUE, print out which elements this one inherits from
#' @examples
#' t <- theme_grey()
#' calc_element('text', t)
#'
#' # Compare the "raw" element definition to the element with calculated inheritance
#' t$axis.text.x
#' calc_element('axis.text.x', t, verbose = TRUE)
#'
#' # This reports that axis.text.x inherits from axis.text, 
#' # which inherits from text. You can view each of them with:
#' t$axis.text.x
#' t$axis.text
#' t$text
#'
#' @export
calc_element <- function(element, theme, verbose = FALSE) {
  if (verbose) message(element, " --> ", appendLF = FALSE)

  # If this is element_blank, don't inherit anything from parents
  if (inherits(theme[[element]], "element_blank")) {
    if (verbose) message("element_blank (no inheritance)")
    return(theme[[element]])
  }

  # If the element is defined (and not just inherited), check that
  # it is of the class specified in .element_tree
  if (!is.null(theme[[element]]) &&
      !inherits(theme[[element]], .element_tree[[element]]$class)) {
    stop(element, " should have class ", .element_tree[[element]]$class)
  }

  # Get the names of parents from the inheritance tree
  pnames <- .element_tree[[element]]$inherit

  # If no parents, this is a "root" node. Just return this element.
  if (is.null(pnames)) {
    # Check that all the properties of this element are non-NULL
    nullprops <- vapply(theme[[element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("Theme element '", element, "' has NULL property: ",
        paste(names(nullprops)[nullprops], collapse = ", "))
    }

    if (verbose) message("nothing (top level)")
    return(theme[[element]])
  }

  # Calculate the parent objects' inheritance
  if (verbose) message(paste(pnames, collapse = ", "))
  parents <- lapply(pnames, calc_element, theme, verbose)

  # If this element is not NULL, then
  # don't try to inherit from parents that are element_blank
  if (!is.null(theme[[element]])) {
    parents <- parents[!vapply(parents, inherits, logical(1), "element_blank")]
  }

  # Combine the properties of this element with all parents
  Reduce(combine_elements, parents, theme[[element]])
}


# Combine the properties of two elements
#
# @param e1 An element object
# @param e2 An element object which e1 inherits from
combine_elements <- function(e1, e2) {

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
