#' Modify components of a theme
#'
#' Use \code{theme()} to modify individual components of a theme, allowing
#' you to control the appearance of all non-data components of the plot.
#' \code{theme()} only affects a single plot: see \code{\link{theme_update}} if
#' you want modify the active theme, to affect all subsequent plots.
#'
#' @section Theme inheritance:
#' Theme elements inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title},
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#' This means that you can modify the appearance of multiple elements by
#' setting a single high-level component.
#'
#' @param line all line elements (\code{element_line})
#' @param rect all rectangular elements (\code{element_rect})
#' @param text all text elements (\code{element_text})
#' @param title all title elements: plot, axes, legends (\code{element_text};
#'   inherits from \code{text})
#' @param aspect.ratio aspect ratio of the panel
#'
#' @param axis.title label of axes (\code{element_text}; inherits from
#'   \code{text})
#' @param axis.title.x x axis label (\code{element_text}; inherits from
#'   \code{axis.title})
#' @param axis.title.x.top x axis label on top axis (\code{element_text};
#'   inherits from \code{axis.title.x})
#' @param axis.title.y y axis label (\code{element_text}; inherits from
#'   \code{axis.title})
#' @param axis.title.y.right y axis label on right axis (\code{element_text};
#'   inherits from \code{axis.title.y})
#' @param axis.text tick labels along axes (\code{element_text}; inherits from
#'   \code{text})
#' @param axis.text.x x axis tick labels (\code{element_text}; inherits from
#'   \code{axis.text})
#' @param axis.text.x.top x axis tick labels on top axis (\code{element_text};
#'   inherits from \code{axis.text.x})
#' @param axis.text.y y axis tick labels (\code{element_text}; inherits from
#'   \code{axis.text})
#' @param axis.text.y.right y axis tick labels on right axis
#'   (\code{element_text}; inherits from \code{axis.text.y})
#' @param axis.ticks tick marks along axes (\code{element_line}; inherits from
#'   \code{line})
#' @param axis.ticks.x x axis tick marks (\code{element_line}; inherits from
#'   \code{axis.ticks})
#' @param axis.ticks.y y axis tick marks (\code{element_line}; inherits from
#'   \code{axis.ticks})
#' @param axis.ticks.length length of tick marks (\code{unit})
#' @param axis.line lines along axes (\code{element_line}; inherits from
#'   \code{line})
#' @param axis.line.x line along x axis (\code{element_line}; inherits from
#'   \code{axis.line})
#' @param axis.line.y line along y axis (\code{element_line}; inherits from
#'   \code{axis.line})
#'
#' @param legend.background background of legend (\code{element_rect}; inherits
#'   from \code{rect})
#' @param legend.margin the margin around each legend (\code{margin})
#' @param legend.spacing the spacing between legends (\code{unit})
#' @param legend.spacing.x the horizontal spacing between legends (\code{unit});
#'   inherits from \code{legend.spacing}
#' @param legend.spacing.y the horizontal spacing between legends (\code{unit});
#'   inherits from \code{legend.spacing}
#' @param legend.key background underneath legend keys (\code{element_rect};
#'   inherits from \code{rect})
#' @param legend.key.size size of legend keys (\code{unit})
#' @param legend.key.height key background height (\code{unit}; inherits from
#'   \code{legend.key.size})
#' @param legend.key.width key background width (\code{unit}; inherits from
#'   \code{legend.key.size})
#' @param legend.text legend item labels (\code{element_text}; inherits from
#'   \code{text})
#' @param legend.text.align alignment of legend labels (number from 0 (left) to
#'   1 (right))
#' @param legend.title title of legend (\code{element_text}; inherits from
#'   \code{title})
#' @param legend.title.align alignment of legend title (number from 0 (left) to
#'   1 (right))
#' @param legend.position the position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector)
#' @param legend.direction layout of items in legends ("horizontal" or
#'   "vertical")
#' @param legend.justification anchor point for positioning legend inside plot
#'   ("center" or two-element numeric vector) or the justification according to
#'   the plot area when positioned outside the plot
#' @param legend.box arrangement of multiple legends ("horizontal" or
#'   "vertical")
#' @param legend.box.just justification of each legend within the overall
#'   bounding box, when there are multiple legends ("top", "bottom", "left", or
#'   "right")
#' @param legend.box.margin margins around the full legend area, as specified
#'   using \code{\link{margin}}
#' @param legend.box.background background of legend area (\code{element_rect};
#'   inherits from \code{rect})
#' @param legend.box.spacing The spacing between the plotting area and the
#'   legend box (\code{unit})
#'
#' @param panel.background background of plotting area, drawn underneath plot
#'   (\code{element_rect}; inherits from \code{rect})
#' @param panel.border border around plotting area, drawn on top of plot so that
#'   it covers tick marks and grid lines. This should be used with
#'   \code{fill=NA}
#' (\code{element_rect}; inherits from \code{rect})
#' @param panel.spacing spacing between facet panels (\code{unit})
#' @param panel.spacing.x horizontal spacing between facet panels (\code{unit};
#'   inherits from \code{panel.spacing})
#' @param panel.spacing.y vertical spacing between facet panels (\code{unit};
#'   inherits from \code{panel.spacing})
#' @param panel.grid grid lines (\code{element_line}; inherits from \code{line})
#' @param panel.grid.major major grid lines (\code{element_line}; inherits from
#'   \code{panel.grid})
#' @param panel.grid.minor minor grid lines (\code{element_line}; inherits from
#' \code{panel.grid})
#' @param panel.grid.major.x vertical major grid lines (\code{element_line};
#'   inherits from \code{panel.grid.major})
#' @param panel.grid.major.y horizontal major grid lines (\code{element_line};
#'   inherits from \code{panel.grid.major})
#' @param panel.grid.minor.x vertical minor grid lines (\code{element_line};
#'   inherits from \code{panel.grid.minor})
#' @param panel.grid.minor.y horizontal minor grid lines (\code{element_line};
#'   inherits from \code{panel.grid.minor})
#' @param panel.ontop option to place the panel (background, gridlines) over
#'   the data layers.  Usually used with a transparent or blank
#'   \code{panel.background}. (\code{logical})
#'
#' @param plot.background background of the entire plot (\code{element_rect};
#'   inherits from \code{rect})
#' @param plot.title plot title (text appearance) (\code{element_text}; inherits
#'   from \code{title}) left-aligned by default
#' @param plot.subtitle plot subtitle (text appearance) (\code{element_text};
#'   inherits from \code{title}) left-aligned by default
#' @param plot.caption caption below the plot (text appearance)
#'   (\code{element_text}; inherits from \code{title}) right-aligned by default
#' @param plot.margin margin around entire plot (\code{unit} with the sizes of
#'   the top, right, bottom, and left margins)
#'
#' @param strip.background background of facet labels (\code{element_rect};
#'   inherits from \code{rect})
#' @param strip.placement placement of strip with respect to axes,
#'    either "inside" or "outside". Only important when axes and strips are
#'    on the same side of the plot.
#' @param strip.text facet labels (\code{element_text}; inherits from
#'   \code{text})
#' @param strip.text.x facet labels along horizontal direction
#'   (\code{element_text}; inherits from \code{strip.text})
#' @param strip.text.y facet labels along vertical direction
#'   (\code{element_text}; inherits from \code{strip.text})
#' @param strip.switch.pad.grid space between strips and axes when strips are
#'   switched (\code{unit})
#' @param strip.switch.pad.wrap space between strips and axes when strips are
#'   switched (\code{unit})
#'
#' @param ... additional element specifications not part of base ggplot2. If
#'   supplied \code{validate} needs to be set to \code{FALSE}.
#' @param complete set this to TRUE if this is a complete theme, such as
#'   the one returned \code{by theme_grey()}. Complete themes behave
#'   differently when added to a ggplot object. Also, when setting
#'   \code{complete = TRUE} all elements will be set to inherit from blank
#'   elements.
#' @param validate \code{TRUE} to run validate_element, \code{FALSE} to bypass checks.
#'
#' @seealso
#'   \code{\link{+.gg}} and \code{\link{\%+replace\%}},
#'   \code{\link{element_blank}}, \code{\link{element_line}},
#'   \code{\link{element_rect}}, and \code{\link{element_text}} for
#'   details of the specific theme elements.
#' @export
#' @examples
#' p1 <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Fuel economy declines as weight increases")
#' p1
#'
#' # Plot ---------------------------------------------------------------------
#' p1 + theme(plot.title = element_text(size = rel(2)))
#' p1 + theme(plot.background = element_rect(fill = "green"))
#'
#' # Panels --------------------------------------------------------------------
#'
#' p1 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
#' p1 + theme(panel.border = element_rect(linetype = "dashed", fill = NA))
#' p1 + theme(panel.grid.major = element_line(colour = "black"))
#' p1 + theme(
#'   panel.grid.major.y = element_blank(),
#'   panel.grid.minor.y = element_blank()
#' )
#'
#' # Put gridlines on top of data
#' p1 + theme(
#'   panel.background = element_rect(fill = NA),
#'   panel.grid.major = element_line(colour = "grey50"),
#'   panel.ontop = TRUE
#' )
#'
#' # Axes ----------------------------------------------------------------------
#' p1 + theme(axis.line = element_line(size = 3, colour = "grey80"))
#' p1 + theme(axis.text = element_text(colour = "blue"))
#' p1 + theme(axis.ticks = element_line(size = 2))
#' p1 + theme(axis.ticks.length = unit(.25, "cm"))
#' p1 + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
#'
#' \donttest{
#' # Legend --------------------------------------------------------------------
#' p2 <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl), shape = factor(vs))) +
#'   labs(
#'     x = "Weight (1000 lbs)",
#'     y = "Fuel economy (mpg)",
#'     colour = "Cylinders",
#'     shape = "Transmission"
#'    )
#' p2
#'
#' # Position
#' p2 + theme(legend.position = "none")
#' p2 + theme(legend.justification = "top")
#' p2 + theme(legend.position = "bottom")
#'
#' # Or place inside the plot using relative coordinates between 0 and 1
#' # legend.justification sets the corner that the position refers to
#' p2 + theme(
#'   legend.position = c(.95, .95),
#'   legend.justification = c("right", "top"),
#'   legend.box.just = "right",
#'   legend.margin = margin(6, 6, 6, 6)
#' )
#'
#' # The legend.box properties work similarly for the space around
#' # all the legends
#' p2 + theme(
#'   legend.box.background = element_rect(),
#'   legend.box.margin = margin(6, 6, 6, 6)
#' )
#'
#' # You can also control the display of the keys
#' # and the justifaction related to the plot area can be set
#' p2 + theme(legend.key = element_rect(fill = "white", colour = "black"))
#' p2 + theme(legend.text = element_text(size = 8, colour = "red"))
#' p2 + theme(legend.title = element_text(face = "bold"))
#'
#' # Strips --------------------------------------------------------------------
#'
#' p3 <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   facet_wrap(~ cyl)
#' p3
#'
#' p3 + theme(strip.background = element_rect(colour = "black", fill = "white"))
#' p3 + theme(strip.text.x = element_text(colour = "white", face = "bold"))
#' p3 + theme(panel.spacing = unit(1, "lines"))
#' }
theme <- function(line,
                  rect,
                  text,
                  title,
                  aspect.ratio,
                  axis.title,
                  axis.title.x,
                  axis.title.x.top,
                  axis.title.y,
                  axis.title.y.right,
                  axis.text,
                  axis.text.x,
                  axis.text.x.top,
                  axis.text.y,
                  axis.text.y.right,
                  axis.ticks,
                  axis.ticks.x,
                  axis.ticks.y,
                  axis.ticks.length,
                  axis.line,
                  axis.line.x,
                  axis.line.y,
                  legend.background,
                  legend.margin,
                  legend.spacing,
                  legend.spacing.x,
                  legend.spacing.y,
                  legend.key,
                  legend.key.size,
                  legend.key.height,
                  legend.key.width,
                  legend.text,
                  legend.text.align,
                  legend.title,
                  legend.title.align,
                  legend.position,
                  legend.direction,
                  legend.justification,
                  legend.box,
                  legend.box.just,
                  legend.box.margin,
                  legend.box.background,
                  legend.box.spacing,
                  panel.background,
                  panel.border,
                  panel.spacing,
                  panel.spacing.x,
                  panel.spacing.y,
                  panel.grid,
                  panel.grid.major,
                  panel.grid.minor,
                  panel.grid.major.x,
                  panel.grid.major.y,
                  panel.grid.minor.x,
                  panel.grid.minor.y,
                  panel.ontop,
                  plot.background,
                  plot.title,
                  plot.subtitle,
                  plot.caption,
                  plot.margin,
                  strip.background,
                  strip.placement,
                  strip.text,
                  strip.text.x,
                  strip.text.y,
                  strip.switch.pad.grid,
                  strip.switch.pad.wrap,
                  ...,
                  complete = FALSE,
                  validate = TRUE
                  ) {
  elements <- find_args(..., complete = NULL, validate = NULL)

  if (!is.null(elements$axis.ticks.margin)) {
    warning("`axis.ticks.margin` is deprecated. Please set `margin` property ",
      " of `axis.text` instead", call. = FALSE)
    elements$axis.ticks.margin <- NULL
  }
  if (!is.null(elements$panel.margin)) {
    warning("`panel.margin` is deprecated. Please use `panel.spacing` property ",
      "instead", call. = FALSE)
    elements$panel.spacing <- elements$panel.margin
    elements$panel.margin <- NULL
  }
  if (!is.null(elements$panel.margin.x)) {
    warning("`panel.margin.x` is deprecated. Please use `panel.spacing.x` property ",
            "instead", call. = FALSE)
    elements$panel.spacing.x <- elements$panel.margin.x
    elements$panel.margin.x <- NULL
  }
  if (!is.null(elements$panel.margin.y)) {
    warning("`panel.margin` is deprecated. Please use `panel.spacing` property ",
            "instead", call. = FALSE)
    elements$panel.spacing.y <- elements$panel.margin.y
    elements$panel.margin.y <- NULL
  }
  if (is.unit(elements$legend.margin) && !is.margin(elements$legend.margin)) {
    warning("`legend.margin` must be specified using `margin()`. For the old ",
      "behavior use legend.spacing", call. = FALSE)
    elements$legend.spacing <- elements$legend.margin
    elements$legend.margin <- margin()
  }

  # Check that all elements have the correct class (element_text, unit, etc)
  if (validate) {
    mapply(validate_element, elements, names(elements))
  }

  # If complete theme set all non-blank elements to inherit from blanks
  if (complete) {
    elements <- lapply(elements, function(el) {
      if (inherits(el, "element") && !inherits(el, "element_blank")) {
        el$inherit.blank <- TRUE
      }
      el
    })
  }
  structure(
    elements,
    class = c("theme", "gg"),
    complete = complete,
    validate = validate
  )
}


# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$theme, theme_get())
}

#' Modify properties of an element in a theme object
#'
#' @param t1 A theme object
#' @param t2 A theme object that is to be added to \code{t1}
#' @param t2name A name of the t2 object. This is used for printing
#'   informative error messages.
#' @keywords internal
add_theme <- function(t1, t2, t2name) {
  if (!is.theme(t2)) {
    stop("Don't know how to add RHS to a theme object",
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
#' @keywords internal
#' @export
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
  if (is.null(e2) || inherits(e1, "element_blank"))  return(e1)
  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) return(e2)
  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) return(e2)
    else return(e1)
  }

  # If e1 has any NULL properties, inherit them from e2
  n <- vapply(e1[names(e2)], is.null, logical(1))
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}

#' Reports whether x is a theme object
#' @param x An object to test
#' @export
#' @keywords internal
is.theme <- function(x) inherits(x, "theme")

#' @export
print.theme <- function(x, ...) utils::str(x)
