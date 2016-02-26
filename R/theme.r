#' Get, set and update themes.
#'
#' Use \code{theme_get} to get the current theme, and \code{theme_set} to
#' completely override it. \code{theme_update} and \code{theme_replace} are
#' shorthands for changing individual elements in the current theme.
#' \code{theme_update} uses the \code{+} operator, so that any unspecified
#' values in the theme element will default to the values they are set in the
#' theme. \code{theme_replace} will completely replace the element, so any
#' unspecified values will overwrite the current value in the theme with \code{NULL}s.
#'
#'
#' @param ... named list of theme settings
#' @seealso \code{\link{\%+replace\%}} and \code{\link{+.gg}}
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' p
#' old <- theme_set(theme_bw())
#' p
#' theme_set(old)
#' p
#'
#' #theme_replace NULLs out the fill attribute of panel.background,
#' #resulting in a white background:
#' theme_get()$panel.background
#' old <- theme_replace(panel.background = element_rect(colour = "pink"))
#' theme_get()$panel.background
#' p
#' theme_set(old)
#'
#' #theme_update only changes the colour attribute, leaving the others intact:
#' old <- theme_update(panel.background = element_rect(colour = "pink"))
#' theme_get()$panel.background
#' p
#' theme_set(old)
#'
#' theme_get()
#'
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(color = mpg)) +
#'   theme(legend.position = c(0.95, 0.95),
#'         legend.justification = c(1, 1))
#' last_plot() +
#'  theme(legend.background = element_rect(fill = "white", colour = "white", size = 3))
#'
theme_update <- function(...) {
  theme_set(theme_get() + theme(...))
}

#' @rdname theme_update
#' @export
theme_replace <- function(...) {
  theme_set(theme_get() %+replace% theme(...))
}

#' Reports whether x is a theme object
#' @param x An object to test
#' @export
is.theme <- function(x) inherits(x, "theme")

#' @export
print.theme <- function(x, ...) utils::str(x)

#' Set theme elements
#'
#'
#' Use this function to modify theme settings.
#'
#' Theme elements can inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title},
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' For more examples of modifying properties using inheritance, see
#' \code{\link{+.gg}} and \code{\link{\%+replace\%}}.
#'
#' To see a graphical representation of the inheritance tree, see the
#' last example below.
#'
#' @section Theme elements:
#' The individual theme elements are:
#'
#' \tabular{ll}{
#'   line             \tab all line elements
#'                    (\code{element_line}) \cr
#'   rect             \tab all rectangular elements
#'                    (\code{element_rect}) \cr
#'   text             \tab all text elements
#'                    (\code{element_text}) \cr
#'   title            \tab all title elements: plot, axes, legends
#'                    (\code{element_text}; inherits from \code{text}) \cr
#'   aspect.ratio     \tab aspect ratio of the panel \cr
#'
#'   axis.title       \tab label of axes
#'                    (\code{element_text}; inherits from \code{text}) \cr
#'   axis.title.x     \tab x axis label
#'                    (\code{element_text}; inherits from \code{axis.title}) \cr
#'   axis.title.y     \tab y axis label
#'                    (\code{element_text}; inherits from \code{axis.title}) \cr
#'   axis.text        \tab tick labels along axes
#'                    (\code{element_text}; inherits from \code{text}) \cr
#'   axis.text.x      \tab x axis tick labels
#'                    (\code{element_text}; inherits from \code{axis.text}) \cr
#'   axis.text.y      \tab y axis tick labels
#'                    (\code{element_text}; inherits from \code{axis.text}) \cr
#'   axis.ticks       \tab tick marks along axes
#'                    (\code{element_line}; inherits from \code{line}) \cr
#'   axis.ticks.x     \tab x axis tick marks
#'                    (\code{element_line}; inherits from \code{axis.ticks}) \cr
#'   axis.ticks.y     \tab y axis tick marks
#'                    (\code{element_line}; inherits from \code{axis.ticks}) \cr
#'   axis.ticks.length  \tab length of tick marks
#'                    (\code{unit}) \cr
#'   axis.line        \tab lines along axes
#'                    (\code{element_line}; inherits from \code{line}) \cr
#'   axis.line.x      \tab line along x axis
#'                    (\code{element_line}; inherits from \code{axis.line}) \cr
#'   axis.line.y      \tab line along y axis
#'                    (\code{element_line}; inherits from \code{axis.line}) \cr
#'
#'   legend.background  \tab background of legend
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   legend.margin    \tab extra space added around legend
#'                    (\code{unit}) \cr
#'   legend.key       \tab background underneath legend keys
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   legend.key.size  \tab size of legend keys
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.key.height  \tab key background height
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.key.width   \tab key background width
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.text      \tab legend item labels
#'                    (\code{element_text}; inherits from \code{text}) \cr
#'   legend.text.align  \tab alignment of legend labels
#'                    (number from 0 (left) to 1 (right)) \cr
#'   legend.title     \tab title of legend
#'                    (\code{element_text}; inherits from \code{title}) \cr
#'   legend.title.align \tab alignment of legend title
#'                    (number from 0 (left) to 1 (right)) \cr
#'   legend.position  \tab the position of legends
#'                    ("none", "left", "right", "bottom", "top", or two-element
#'                      numeric vector) \cr
#'   legend.direction \tab layout of items in legends
#'                    ("horizontal" or "vertical") \cr
#'   legend.justification \tab anchor point for positioning legend inside plot
#'                    ("center" or two-element numeric vector) \cr
#'   legend.box       \tab arrangement of multiple legends
#'                    ("horizontal" or "vertical") \cr
#'   legend.box.just  \tab justification of each legend within the overall
#'                    bounding box, when there are multiple legends
#'                    ("top", "bottom", "left", or "right")\cr
#'
#'   panel.background \tab background of plotting area, drawn underneath plot
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   panel.border     \tab border around plotting area, drawn on top of plot
#'                    so that it covers tick marks and grid lines. This should
#'                    be used with \code{fill=NA}
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   panel.margin     \tab margin around facet panels
#'                    (\code{unit}) \cr
#'   panel.margin.x   \tab horizontal margin around facet panels
#'                    (\code{unit}; inherits from \code{panel.margin}) \cr
#'   panel.margin.y   \tab vertical margin around facet panels
#'                    (\code{unit}; inherits from \code{panel.margin}) \cr
#'   panel.grid       \tab grid lines
#'                    (\code{element_line}; inherits from \code{line}) \cr
#'   panel.grid.major \tab major grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid}) \cr
#'   panel.grid.minor \tab minor grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid}) \cr
#'   panel.grid.major.x \tab vertical major grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid.major}) \cr
#'   panel.grid.major.y \tab horizontal major grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid.major}) \cr
#'   panel.grid.minor.x \tab vertical minor grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid.minor}) \cr
#'   panel.grid.minor.y \tab horizontal minor grid lines
#'                    (\code{element_line}; inherits from \code{panel.grid.minor}) \cr
#'   panel.ontop        \tab option to place the panel (background, gridlines)
#'                           over the data layers.  Usually used with a transparent
#'                           or blank \code{panel.background}. (\code{logical}) \cr
#'
#'   plot.background  \tab background of the entire plot
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   plot.title       \tab plot title (text appearance)
#'                    (\code{element_text}; inherits from \code{title}) \cr
#'   plot.margin      \tab margin around entire plot
#'                    (\code{unit} with the sizes of the top, right, bottom, and
#'                     left margins) \cr
#'
#'   strip.background \tab background of facet labels
#'                    (\code{element_rect}; inherits from \code{rect}) \cr
#'   strip.text       \tab facet labels
#'                    (\code{element_text}; inherits from \code{text}) \cr
#'   strip.text.x     \tab facet labels along horizontal direction
#'                    (\code{element_text}; inherits from \code{strip.text}) \cr
#'   strip.text.y     \tab facet labels along vertical direction
#'                    (\code{element_text}; inherits from \code{strip.text}) \cr
#'   strip.switch.pad.grid \tab space between strips and axes when strips are switched
#'                    (\code{unit}) \cr
#'   strip.switch.pad.wrap \tab space between strips and axes when strips are switched
#'                    (\code{unit}) \cr
#' }
#'
#' @param ... a list of element name, element pairings that modify the
#'   existing theme.
#' @param complete set this to TRUE if this is a complete theme, such as
#'   the one returned \code{by theme_grey()}. Complete themes behave
#'   differently when added to a ggplot object.
#' @param validate TRUE to run validate_element, FALSE to bypass checks.
#'
#' @seealso \code{\link{+.gg}}
#' @seealso \code{\link{\%+replace\%}}
#' @seealso \code{\link{rel}}
#' @seealso \code{\link{element_blank}}
#' @seealso \code{\link{element_line}}
#' @seealso \code{\link{element_rect}}
#' @seealso \code{\link{element_text}}
#' @export
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' p
#' p + theme(panel.background = element_rect(colour = "pink"))
#' p + theme_bw()
#'
#' # Scatter plot of gas mileage by vehicle weight
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
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
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#' # Or
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#'
#' # Change title appearance
#' p <- p + labs(title = "Vehicle Weight-Gas Mileage Relationship")
#' # Set title to twice the base font size
#' p + theme(plot.title = element_text(size = rel(2)))
#' p + theme(plot.title = element_text(size = rel(2), colour = "blue"))
#'
#' # Changing plot look with themes
#' DF <- data.frame(x = rnorm(400))
#' m <- ggplot(DF, aes(x = x)) +
#'   geom_histogram()
#' # Default is theme_grey()
#' m
#' # Compare with
#' m + theme_bw()
#'
#' # Manipulate Axis Attributes
#' m + theme(axis.line = element_line(size = 3, colour = "red", linetype = "dotted"))
#' m + theme(axis.text = element_text(colour = "blue"))
#' m + theme(axis.text.y = element_blank())
#' m + theme(axis.ticks = element_line(size = 2))
#' m + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
#' m + theme(axis.title.x = element_blank())
#' m + theme(axis.ticks.length = unit(.85, "cm"))
#'
#' # Legend Attributes
#' z <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl)))
#' z
#' z + theme(legend.position = "none")
#' z + theme(legend.position = "bottom")
#' # Or use relative coordinates between 0 and 1
#' z + theme(legend.position = c(.5, .5))
#' # Add a border to the whole legend
#' z + theme(legend.background = element_rect(colour = "black"))
#' # Legend margin controls extra space around outside of legend:
#' z + theme(legend.background = element_rect(),
#'           legend.margin = unit(1, "cm"))
#' z + theme(legend.background = element_rect(),
#'           legend.margin = unit(0, "cm"))
#' # Or to just the keys
#' z + theme(legend.key = element_rect(colour = "black"))
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
#' z + theme(panel.background = element_rect(fill = "black"))
#' z + theme(panel.border = element_rect(linetype = "dashed", colour = "black"))
#' z + theme(panel.grid.major = element_line(colour = "blue"))
#' z + theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"))
#' z + theme(panel.grid.major = element_line(size = 2))
#' z + theme(panel.grid.major.y = element_blank(),
#'           panel.grid.minor.y = element_blank())
#' z + theme(plot.background = element_rect())
#' z + theme(plot.background = element_rect(fill = "green"))
#'
#' # Faceting Attributes
#' set.seed(4940)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' k <- ggplot(dsmall, aes(carat, ..density..)) +
#'   geom_histogram(binwidth = 0.2) +
#'   facet_grid(. ~ cut)
#' k + theme(strip.background = element_rect(colour = "purple", fill = "pink",
#'                                           size = 3, linetype = "dashed"))
#' k + theme(strip.text.x = element_text(colour = "red", angle = 45, size = 10,
#'                                       hjust = 0.5, vjust = 0.5))
#' k + theme(panel.margin = unit(5, "lines"))
#' k + theme(panel.margin.y = unit(0, "lines"))
#'
#' # Put gridlines on top
#' meanprice <- tapply(diamonds$price, diamonds$cut, mean)
#' cut <- factor(levels(diamonds$cut), levels = levels(diamonds$cut))
#' df <- data.frame(meanprice, cut)
#' g <- ggplot(df, aes(cut, meanprice)) + geom_bar(stat = "identity")
#' g + geom_bar(stat = "identity") +
#'     theme(panel.background = element_blank(),
#'           panel.grid.major.x = element_blank(),
#'           panel.grid.minor.x = element_blank(),
#'           panel.grid.minor.y = element_blank(),
#'           panel.ontop = TRUE)
#'
#' # Modify a theme and save it
#' mytheme <- theme_grey() + theme(plot.title = element_text(colour = "red"))
#' p + mytheme
#'
#' }
theme <- function(..., complete = FALSE, validate = TRUE) {
  elements <- list(...)

  if (!is.null(elements$axis.ticks.margin)) {
    warning("`axis.ticks.margin` is deprecated. Please set `margin` property ",
      " of `axis.text` instead", call. = FALSE)
    elements$axis.ticks.margin <- NULL
  }

  # Check that all elements have the correct class (element_text, unit, etc)
  if (validate) {
    mapply(validate_element, elements, names(elements))
  }

  structure(elements, class = c("theme", "gg"),
            complete = complete, validate = validate)
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


#' @rdname gg-add
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
#' @param t1 A theme object
#' @param t2 A theme object that is to be added to \code{t1}
#' @param t2name A name of the t2 object. This is used for printing
#'   informative error messages.
#'
#' @seealso +.gg
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

    if (is.null(x) || inherits(x, "element_blank")) {
      # If x is NULL or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) ||
               is.logical(y) || inherits(y, "element_blank")) {
      # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
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
#   ggplot(data.frame(x = 1:3, y = 1:3)) +
#   geom_point() + theme(text = element_text(colour = 'red'))
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
  newitems <- !names(newtheme) %in% names(oldtheme)
  newitem_names <- names(newtheme)[newitems]
  oldtheme[newitem_names] <- theme_get()[newitem_names]

  # Update the theme elements with the things from newtheme
  # Turn the 'theme' list into a proper theme object first, and preserve
  # the 'complete' attribute. It's possible that oldtheme is an empty
  # list, and in that case, set complete to FALSE.
  old.validate <- isTRUE(attr(oldtheme, "validate"))
  new.validate <- isTRUE(attr(newtheme, "validate"))
  oldtheme <- do.call(theme, c(oldtheme,
    complete = isTRUE(attr(oldtheme, "complete")),
    validate = old.validate & new.validate))

  oldtheme + newtheme
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

  # If e1 is NULL, or if e2 is element_blank, inherit everything from e2
  if (is.null(e1) || inherits(e2, "element_blank"))  return(e2)

  # If e1 has any NULL properties, inherit them from e2
  n <- vapply(e1[names(e2)], is.null, logical(1))
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}
