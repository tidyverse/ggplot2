#' Modify components of a theme
#'
#' Themes are a powerful way to customize the non-data components of your plots:
#' i.e. titles, labels, fonts, background, gridlines, and legends. Themes can be
#' used to give plots a consistent customized look. Modify a single plot's theme
#' using `theme()`; see [theme_update()] if you want modify the active theme, to
#' affect all subsequent plots. Use the themes available in [complete
#' themes][theme_bw] if you would like to use a complete theme such as
#' `theme_bw()`, `theme_minimal()`, and more. Theme elements are documented
#' together according to inheritance, read more about theme inheritance below.
#'
#' @section Theme inheritance:
#' Theme elements inherit properties from other theme elements hierarchically.
#' For example, `axis.title.x.bottom` inherits from `axis.title.x` which inherits
#' from `axis.title`, which in turn inherits from `text`. All text elements inherit
#' directly or indirectly from `text`; all lines inherit from
#' `line`, and all rectangular objects inherit from `rect`.
#' This means that you can modify the appearance of multiple elements by
#' setting a single high-level component.
#'
#' Learn more about setting these aesthetics in `vignette("ggplot2-specs")`.
#'
#' @param line all line elements ([element_line()])
#' @param rect all rectangular elements ([element_rect()])
#' @param text all text elements ([element_text()])
#' @param title all title elements: plot, axes, legends ([element_text()];
#'   inherits from `text`)
#' @param aspect.ratio aspect ratio of the panel
#'
#' @param axis.title,axis.title.x,axis.title.y,axis.title.x.top,axis.title.x.bottom,axis.title.y.left,axis.title.y.right
#'   labels of axes ([element_text()]). Specify all axes' labels (`axis.title`),
#'   labels by plane (using `axis.title.x` or `axis.title.y`), or individually
#'   for each axis (using `axis.title.x.bottom`, `axis.title.x.top`,
#'   `axis.title.y.left`, `axis.title.y.right`). `axis.title.*.*` inherits from
#'   `axis.title.*` which inherits from `axis.title`, which in turn inherits
#'   from `text`
#' @param axis.text,axis.text.x,axis.text.y,axis.text.x.top,axis.text.x.bottom,axis.text.y.left,axis.text.y.right
#'   tick labels along axes ([element_text()]). Specify all axis tick labels (`axis.text`),
#'   tick labels by plane (using `axis.text.x` or `axis.text.y`), or individually
#'   for each axis (using `axis.text.x.bottom`, `axis.text.x.top`,
#'   `axis.text.y.left`, `axis.text.y.right`). `axis.text.*.*` inherits from
#'   `axis.text.*` which inherits from `axis.text`, which in turn inherits
#'   from `text`
#' @param axis.ticks,axis.ticks.x,axis.ticks.x.top,axis.ticks.x.bottom,axis.ticks.y,axis.ticks.y.left,axis.ticks.y.right
#'   tick marks along axes ([element_line()]). Specify all tick marks (`axis.ticks`),
#'   ticks by plane (using `axis.ticks.x` or `axis.ticks.y`), or individually
#'   for each axis (using `axis.ticks.x.bottom`, `axis.ticks.x.top`,
#'   `axis.ticks.y.left`, `axis.ticks.y.right`). `axis.ticks.*.*` inherits from
#'   `axis.ticks.*` which inherits from `axis.ticks`, which in turn inherits
#'   from `line`
#' @param axis.ticks.length,axis.ticks.length.x,axis.ticks.length.x.top,axis.ticks.length.x.bottom,axis.ticks.length.y,axis.ticks.length.y.left,axis.ticks.length.y.right
#' length of tick marks (`unit`)
#' @param axis.line,axis.line.x,axis.line.x.top,axis.line.x.bottom,axis.line.y,axis.line.y.left,axis.line.y.right
#'   lines along axes ([element_line()]). Specify lines along all axes (`axis.line`),
#'   lines for each plane (using `axis.line.x` or `axis.line.y`), or individually
#'   for each axis (using `axis.line.x.bottom`, `axis.line.x.top`,
#'   `axis.line.y.left`, `axis.line.y.right`). `axis.line.*.*` inherits from
#'   `axis.line.*` which inherits from `axis.line`, which in turn inherits
#'   from `line`
#'
#' @param legend.background background of legend ([element_rect()]; inherits
#'   from `rect`)
#' @param legend.margin the margin around each legend ([margin()])
#' @param legend.spacing,legend.spacing.x,legend.spacing.y
#'   the spacing between legends (`unit`). `legend.spacing.x` & `legend.spacing.y`
#'   inherit from `legend.spacing` or can be specified separately
#' @param legend.key background underneath legend keys ([element_rect()];
#'   inherits from `rect`)
#' @param legend.key.size,legend.key.height,legend.key.width
#'   size of legend keys (`unit`); key background height & width inherit from
#'   `legend.key.size` or can be specified separately
#' @param legend.text legend item labels ([element_text()]; inherits from
#'   `text`)
#' @param legend.text.align alignment of legend labels (number from 0 (left) to
#'   1 (right))
#' @param legend.title title of legend ([element_text()]; inherits from
#'   `title`)
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
#'   using [margin()]
#' @param legend.box.background background of legend area ([element_rect()];
#'   inherits from `rect`)
#' @param legend.box.spacing The spacing between the plotting area and the
#'   legend box (`unit`)
#'
#' @param panel.background background of plotting area, drawn underneath plot
#'   ([element_rect()]; inherits from `rect`)
#' @param panel.border border around plotting area, drawn on top of plot so that
#'   it covers tick marks and grid lines. This should be used with
#'   `fill = NA`
#'   ([element_rect()]; inherits from `rect`)
#' @param panel.spacing,panel.spacing.x,panel.spacing.y spacing between facet
#'   panels (`unit`). `panel.spacing.x` & `panel.spacing.y` inherit from `panel.spacing`
#'   or can be specified separately.
#' @param panel.grid,panel.grid.major,panel.grid.minor,panel.grid.major.x,panel.grid.major.y,panel.grid.minor.x,panel.grid.minor.y
#'   grid lines ([element_line()]). Specify major grid lines,
#'   or minor grid lines separately (using `panel.grid.major` or `panel.grid.minor`)
#'   or individually for each axis (using `panel.grid.major.x`, `panel.grid.minor.x`,
#'   `panel.grid.major.y`, `panel.grid.minor.y`).  Y axis grid lines are horizontal
#'   and x axis grid lines are vertical. `panel.grid.*.*` inherits from
#'   `panel.grid.*` which inherits from `panel.grid`, which in turn inherits
#'   from `line`
#' @param panel.ontop option to place the panel (background, gridlines) over
#'   the data layers (`logical`). Usually used with a transparent or blank
#'   `panel.background`.
#'
#' @param plot.background background of the entire plot ([element_rect()];
#'   inherits from `rect`)
#' @param plot.title plot title (text appearance) ([element_text()]; inherits
#'   from `title`) left-aligned by default
#' @param plot.subtitle plot subtitle (text appearance) ([element_text()];
#'   inherits from `title`) left-aligned by default
#' @param plot.caption caption below the plot (text appearance)
#'   ([element_text()]; inherits from `title`) right-aligned by default
#' @param plot.title.position,plot.caption.position Alignment of the plot title/subtitle
#'   and caption.  The setting for `plot.title.position` applies to both
#'   the title and the subtitle. A value of "panel" (the default) means that
#'   titles and/or caption are aligned to the plot panels. A value of "plot" means
#'   that titles and/or caption are aligned to the entire plot (minus any space
#'   for margins and plot tag).
#' @param plot.tag upper-left label to identify a plot (text appearance)
#'   ([element_text()]; inherits from `title`) left-aligned by default
#' @param plot.tag.position The position of the tag as a string ("topleft",
#'   "top", "topright", "left", "right", "bottomleft", "bottom", "bottomright)
#'   or a coordinate. If a string, extra space will be added to accommodate the
#'   tag.
#' @param plot.margin margin around entire plot (`unit` with the sizes of
#'   the top, right, bottom, and left margins)
#'
#' @param strip.background,strip.background.x,strip.background.y
#'   background of facet labels ([element_rect()];
#'   inherits from `rect`). Horizontal facet background (`strip.background.x`)
#'   & vertical facet background (`strip.background.y`) inherit from
#'   `strip.background` or can be specified separately
#' @param strip.placement placement of strip with respect to axes,
#'    either "inside" or "outside". Only important when axes and strips are
#'    on the same side of the plot.
#' @param strip.text,strip.text.x,strip.text.y facet labels ([element_text()];
#'   inherits from  `text`). Horizontal facet labels (`strip.text.x`) & vertical
#'   facet labels (`strip.text.y`) inherit from `strip.text` or can be specified
#'   separately
#' @param strip.switch.pad.grid space between strips and axes when strips are
#'   switched (`unit`)
#' @param strip.switch.pad.wrap space between strips and axes when strips are
#'   switched (`unit`)
#'
#' @param ... additional element specifications not part of base ggplot2. In general,
#'   these should also be defined in the `element tree` argument.
#' @param complete set this to `TRUE` if this is a complete theme, such as
#'   the one returned by [theme_grey()]. Complete themes behave
#'   differently when added to a ggplot object. Also, when setting
#'   `complete = TRUE` all elements will be set to inherit from blank
#'   elements.
#' @param validate `TRUE` to run `validate_element()`, `FALSE` to bypass checks.
#' @seealso
#'   [+.gg()] and [%+replace%],
#'   [element_blank()], [element_line()],
#'   [element_rect()], and [element_text()] for
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
#' # Change styles of axes texts and lines
#' p1 + theme(axis.line = element_line(size = 3, colour = "grey80"))
#' p1 + theme(axis.text = element_text(colour = "blue"))
#' p1 + theme(axis.ticks = element_line(size = 2))
#'
#' # Change the appearance of the y-axis title
#' p1 + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
#'
#' # Make ticks point outwards on y-axis and inwards on x-axis
#' p1 + theme(
#'   axis.ticks.length.y = unit(.25, "cm"),
#'   axis.ticks.length.x = unit(-.25, "cm"),
#'   axis.text.x = element_text(margin = margin(t = .3, unit = "cm"))
#' )
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
#' # Or place legends inside the plot using relative coordinates between 0 and 1
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
#' # and the justification related to the plot area can be set
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
                  axis.title.x.bottom,
                  axis.title.y,
                  axis.title.y.left,
                  axis.title.y.right,
                  axis.text,
                  axis.text.x,
                  axis.text.x.top,
                  axis.text.x.bottom,
                  axis.text.y,
                  axis.text.y.left,
                  axis.text.y.right,
                  axis.ticks,
                  axis.ticks.x,
                  axis.ticks.x.top,
                  axis.ticks.x.bottom,
                  axis.ticks.y,
                  axis.ticks.y.left,
                  axis.ticks.y.right,
                  axis.ticks.length,
                  axis.ticks.length.x,
                  axis.ticks.length.x.top,
                  axis.ticks.length.x.bottom,
                  axis.ticks.length.y,
                  axis.ticks.length.y.left,
                  axis.ticks.length.y.right,
                  axis.line,
                  axis.line.x,
                  axis.line.x.top,
                  axis.line.x.bottom,
                  axis.line.y,
                  axis.line.y.left,
                  axis.line.y.right,
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
                  plot.title.position,
                  plot.subtitle,
                  plot.caption,
                  plot.caption.position,
                  plot.tag,
                  plot.tag.position,
                  plot.margin,
                  strip.background,
                  strip.background.x,
                  strip.background.y,
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
    warn("`axis.ticks.margin` is deprecated. Please set `margin` property of `axis.text` instead")
    elements$axis.ticks.margin <- NULL
  }
  if (!is.null(elements$panel.margin)) {
    warn("`panel.margin` is deprecated. Please use `panel.spacing` property instead")
    elements$panel.spacing <- elements$panel.margin
    elements$panel.margin <- NULL
  }
  if (!is.null(elements$panel.margin.x)) {
    warn("`panel.margin.x` is deprecated. Please use `panel.spacing.x` property instead")
    elements$panel.spacing.x <- elements$panel.margin.x
    elements$panel.margin.x <- NULL
  }
  if (!is.null(elements$panel.margin.y)) {
    warn("`panel.margin` is deprecated. Please use `panel.spacing` property instead")
    elements$panel.spacing.y <- elements$panel.margin.y
    elements$panel.margin.y <- NULL
  }
  if (is.unit(elements$legend.margin) && !is.margin(elements$legend.margin)) {
    warn("`legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing")
    elements$legend.spacing <- elements$legend.margin
    elements$legend.margin <- margin()
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

# check whether theme is complete
is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

# check whether theme should be validated
is_theme_validate <- function(x) {
  validate <- attr(x, "validate", exact = TRUE)
  if (is.null(validate))
    TRUE # we validate by default
  else
    isTRUE(validate)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x, default = theme_get()) {
  theme <- x$theme

  # apply theme defaults appropriately if needed
  if (is_theme_complete(theme)) {
    # for complete themes, we fill in missing elements but don't do any element merging
    # can't use `defaults()` because it strips attributes
    missing <- setdiff(names(default), names(theme))
    theme[missing] <- default[missing]
  } else {
    # otherwise, we can just add the theme to the default theme
    theme <- default + theme
  }

  # if we're still missing elements relative to fallback default, fill in those
  missing <- setdiff(names(ggplot_global$theme_default), names(theme))
  theme[missing] <- ggplot_global$theme_default[missing]

  # Check that all elements have the correct class (element_text, unit, etc)
  if (is_theme_validate(theme)) {
    mapply(
      validate_element, theme, names(theme),
      MoreArgs = list(element_tree = get_element_tree())
    )
  }

  theme
}

#' Modify properties of an element in a theme object
#'
#' @param t1 A theme object
#' @param t2 A theme object that is to be added to `t1`
#' @param t2name A name of the t2 object. This is used for printing
#'   informative error messages.
#' @keywords internal
add_theme <- function(t1, t2, t2name) {
  if (!is.list(t2)) { # in various places in the code base, simple lists are used as themes
    abort(glue("Can't add `{t2name}` to a theme object."))
  }

  # If t2 is a complete theme or t1 is NULL, just return t2
  if (is_theme_complete(t2) || is.null(t1))
    return(t2)

  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- merge_element(t2[[item]], t1[[item]])

    # Assign it back to t1
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }

  # make sure the "complete" attribute is set; this can be missing
  # when t1 is an empty list
  attr(t1, "complete") <- is_theme_complete(t1)

  # Only validate if both themes should be validated
  attr(t1, "validate") <-
    is_theme_validate(t1) && is_theme_validate(t2)

  t1
}


#' Calculate the element properties, by inheriting properties from its parents
#'
#' @param element The name of the theme element to calculate
#' @param theme A theme object (like [theme_grey()])
#' @param verbose If TRUE, print out which elements this one inherits from
#' @param skip_blank If TRUE, elements of type `element_blank` in the
#'   inheritance hierarchy will be ignored.
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
calc_element <- function(element, theme, verbose = FALSE, skip_blank = FALSE) {
  if (verbose) message(element, " --> ", appendLF = FALSE)

  el_out <- theme[[element]]

  # If result is element_blank, we skip it if `skip_blank` is `TRUE`,
  # and otherwise we don't inherit anything from parents
  if (inherits(el_out, "element_blank")) {
    if (isTRUE(skip_blank)) {
      el_out <- NULL
    } else {
      if (verbose) message("element_blank (no inheritance)")
      return(el_out)
    }
  }

  # Obtain the element tree
  element_tree <- get_element_tree()

  # If the element is defined (and not just inherited), check that
  # it is of the class specified in element_tree
  if (!is.null(el_out) &&
      !inherits(el_out, element_tree[[element]]$class)) {
    abort(glue("{element} should have class {ggplot_global$element_tree[[element]]$class}"))
  }

  # Get the names of parents from the inheritance tree
  pnames <- element_tree[[element]]$inherit

  # If no parents, this is a "root" node. Just return this element.
  if (is.null(pnames)) {
    if (verbose) message("nothing (top level)")

    # Check that all the properties of this element are non-NULL
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties, return element as is
    }

    # if we have null properties, try to fill in from ggplot_global$theme_default
    el_out <- combine_elements(el_out, ggplot_global$theme_default[[element]])
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties remaining, return element
    }

    abort(glue("Theme element `{element}` has NULL property without default: ",
          glue_collapse(names(nullprops)[nullprops], ", ", last = " and ")))
  }

  # Calculate the parent objects' inheritance
  if (verbose) message(paste(pnames, collapse = ", "))
  parents <- lapply(
    pnames,
    calc_element,
    theme,
    verbose = verbose,
    # once we've started skipping blanks, we continue doing so until the end of the
    # recursion; we initiate skipping blanks if we encounter an element that
    # doesn't inherit blank.
    skip_blank = skip_blank || (!is.null(el_out) && !isTRUE(el_out$inherit.blank))
  )

  # Combine the properties of this element with all parents
  Reduce(combine_elements, parents, el_out)
}

#' Merge a parent element into a child element
#'
#' This is a generic and element classes must provide an implementation of this
#' method
#'
#' @param new The child element in the theme hierarchy
#' @param old The parent element in the theme hierarchy
#' @return A modified version of `new` updated with the properties of
#' `old`
#' @keywords internal
#' @export
#' @examples
#' new <- element_text(colour = "red")
#' old <- element_text(colour = "blue", size = 10)
#'
#' # Adopt size but ignore colour
#' merge_element(new, old)
#'
merge_element <- function(new, old) {
  UseMethod("merge_element")
}

#' @rdname merge_element
#' @export
merge_element.default <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  } else if (is.null(new) || is.character(new) || is.numeric(new) || is.unit(new) ||
             is.logical(new)) {
    # If new is NULL, or a string, numeric vector, unit, or logical, just return it
    return(new)
  }

  # otherwise we can't merge
  abort(glue("No method for merging {class(new)[1]} into {class(old)[1]}"))
}

#' @rdname merge_element
#' @export
merge_element.element_blank <- function(new, old) {
  # If new is element_blank, just return it
  new
}

#' @rdname merge_element
#' @export
merge_element.element <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  }

  # actual merging can only happen if classes match
  if (!inherits(new, class(old)[1])) {
    abort("Only elements of the same class can be merged")
  }

  # Override NULL properties of new with the values in old
  # Get logical vector of NULL properties in new
  idx <- vapply(new, is.null, logical(1))
  # Get the names of TRUE items
  idx <- names(idx[idx])

  # Update non-NULL items
  new[idx] <- old[idx]

  new
}

#' Combine the properties of two elements
#'
#' @param e1 An element object
#' @param e2 An element object from which e1 inherits
#'
#' @noRd
#'
combine_elements <- function(e1, e2) {

  # If e2 is NULL, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }

  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) {
    return(e2)
  }

  # If neither of e1 or e2 are element_* objects, return e1
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }

  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }

  # If e1 has any NULL properties, inherit them from e2
  n <- names(e1)[vapply(e1, is.null, logical(1))]
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
