#' Labeller functions
#'
#' Labeller functions are in charge of formatting the strip labels of
#' facet grids and wraps. Most of them accept a \code{multi_line}
#' argument to control whether multiple factors (defined in formulae
#' such as \code{~first + second}) should be displayed on a single
#' line separated with commas, or each on their own line.
#'
#' \code{label_value()} only displays the value of a factor while
#' \code{label_both()} displays both the variable name and the factor
#' value. \code{label_context()} is context-dependent and uses
#' \code{label_value()} for single factor facetting and
#' \code{label_both()} when multiple factors are
#' involved. \code{label_wrap_gen()} uses \code{\link[base]{strwrap}()}
#' for line wrapping.
#'
#' \code{\link{label_parsed}()} interprets the labels as plotmath
#' expressions. \code{\link{label_bquote}()} offers a more flexible
#' way of constructing plotmath expressions. See examples and
#' \code{\link{bquote}()} for details on the syntax of the
#' argument.
#'
#' @section Writing New Labeller Functions:
#'
#'   A labeller function accepts a data frame of labels (character
#'   vectors) containing one column for each factor. Multiple factors
#'   occur with formula of the type \code{~first + second}.
#'
#'   The return value must be a rectangular list where each 'row'
#'   characterises a single facet. The list elements can be either
#'   character vectors or lists of plotmath expressions. When multiple
#'   elements are returned, they get displayed on their own new lines
#'   (i.e., each facet gets a multi-line strip of labels).
#'
#'   To illustrate, let's say your labeller returns a list of two
#'   character vectors of length 3. This is a rectangular list because
#'   all elements have the same length. The first facet will get the
#'   first elements of each vector and display each of them on their
#'   own line. Then the second facet gets the second elements of each
#'   vector, and so on.
#'
#'   For compatibility with \code{\link{labeller}()}, each labeller
#'   function must have the \code{labeller} S3 class.
#'
#' @param labels Data frame of labels. Usually contains only one
#'   element, but facetting over multiple factors entails multiple
#'   label variables.
#' @param multi_line Whether to display the labels of multiple factors
#'   on separate lines.
#' @param sep String separating variables and values.
#' @param expr Backquoted labelling expression.
#' @param width Maximum number of characters before wrapping the strip.
#' @family facet
#' @seealso \code{\link{labeller}()}
#' @name labellers
#' @examples
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Displaying only the values
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(. ~ cyl, labeller = label_value)
#'
#' # Displaying both the values and the variables
#' p + facet_grid(. ~ cyl, labeller = label_both)
#'
#' # Displaying only the values or both the values and variables
#' # depending on whether multiple factors are facetted over
#' p + facet_grid(am ~ vs+cyl, labeller = label_context)
#'
#' # Interpreting the labels as plotmath expressions
#' p + facet_grid(. ~ cyl2)
#' p + facet_grid(. ~ cyl2, labeller = label_parsed)
#' p + facet_wrap(~vs + cyl2, labeller = label_parsed)
#'
#' # You can also provide a flexible backquoted expression. The labels
#' # must be backquoted and referred to by their names.
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(vs)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(.(vs) ^ .(vs)))
#' p + facet_grid(. ~ vs + am, labeller = label_bquote(.(am) ^ .(vs)))
NULL

#' @rdname labellers
#' @export
label_value <- function(labels, multi_line = TRUE) {
  labels <- lapply(labels, as.character)
  if (multi_line) {
    labels
  } else {
    out <- do.call("Map", c(list(paste, sep = ", "), labels))
    list(unname(unlist(out)))
  }
}
# Should ideally not have the 'function' class here, but this is
# currently needed for Roxygen
class(label_value) <- c("function", "labeller")

# Helper for label_both
label_variable <- function(labels, multi_line = TRUE) {
  if (multi_line) {
    row <- as.list(names(labels))
  } else {
    row <- list(paste(names(labels), collapse = ", "))
  }
  lapply(row, rep, nrow(labels) %||% length(labels[[1]]))
}

#' @rdname labellers
#' @export
label_both <- function(labels, multi_line = TRUE, sep = ": ") {
  value <- label_value(labels, multi_line = multi_line)
  variable <- label_variable(labels, multi_line = multi_line)

  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
    }
  } else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }

  out
}
class(label_both) <- c("function", "labeller")

#' @rdname labellers
#' @export
label_context <- function(labels, multi_line = TRUE, sep = ": ") {
  if (length(labels) == 1) {
    label_value(labels, multi_line)
  } else {
    label_both(labels, multi_line)
  }
}
class(label_context) <- c("function", "labeller")

#' @rdname labellers
#' @export
label_parsed <- function(labels, multi_line = TRUE) {
  labels <- label_value(labels, multi_line = multi_line)
  if (multi_line) {
    # Using unname() and c() to return a cleaner and easily testable
    # object structure
    lapply(unname(labels), lapply, function(values) {
      c(parse(text = as.character(values)))
    })
  } else {
    lapply(labels, function(values) {
      values <- paste0("list(", values, ")")
      lapply(values, function(expr) c(parse(text = expr)))
    })
  }
}
class(label_parsed) <- c("function", "labeller")

#' @rdname labellers
#' @export
label_bquote <- function(expr) {
  quoted <- substitute(expr)
  has_warned <- FALSE

  fun <- function(labels) {
    evaluate <- function(...) {
      params <- list(...)

      # Mapping `x` to the first variable for backward-compatibility,
      # but only if there is no facetted variable also named `x`
      if (!"x" %in% names(params)) {
        if (!has_warned) {
          warning("Referring to `x` is deprecated, use variable name instead",
            call. = FALSE)
          # The function is called for each facet so this avoids
          # multiple warnings
          has_warned <<- TRUE
        }
        params$x <- params[[1]]
      }

      eval(substitute(bquote(expr, params), list(expr = quoted)))
    }
    list(do.call("Map", c(list(f = evaluate), labels)))
  }

  structure(fun, class = "labeller")
}
globalVariables(c("x", "."))

#' @rdname labellers
#' @export
label_wrap_gen <- function(width = 25, multi_line = TRUE) {
  fun <- function(labels) {
    labels <- label_value(labels, multi_line = multi_line)
    lapply(labels, function(x) {
      x <- strwrap(x, width = width, simplify = FALSE)
      vapply(x, paste, character(1), collapse = "\n")
    })
  }
  structure(fun, class = "labeller")
}

is_labeller <- function(x) inherits(x, "labeller")

#' Generic labeller function for facets
#'
#' This function makes it easy to assign different labellers to
#' different factors. The labeller can be a function or it can be a
#' named character vectors that will serve as a lookup table.
#'
#' In case of functions, if the labeller has class \code{labeller}, it
#' is directly applied on the data frame of labels. Otherwise, it is
#' applied to the columns of the data frame of labels. The data frame
#' is then processed with the function specified in the
#' \code{.default} argument. This is intended to be used with
#' functions taking a character vector such as
#' \code{\link[Hmisc]{capitalize}}.
#'
#' @param ... Named arguments of the form \code{variable = labeller},
#'   where \code{values} could be a vector or method.
#' @param keep.as.numeric Deprecated, use \code{.as_character} instead.
#' @param .as_character Logical, default FALSE. When TRUE, converts
#'   numeric labels to characters.
#' @param .multi_line Whether to display the labels of multiple
#'   factors on separate lines. This is passed to the labeller function.
#' @param .default Default labeller function with which to process the
#'   labels not mentioned in \code{...} or those produced with a
#'   lookup table or a non-labeller function.
#' @inheritParams label_value
#' @family facet labeller
#' @seealso \link{labellers}
#' @return A labeller function to supply to \code{\link{facet_grid}}
#'   for the argument \code{labeller}.
#' @export
#' @examples
#' \donttest{
#' p1 <- ggplot(mpg, aes(cty, hwy)) + geom_point()
#' p1 + facet_grid(cyl ~ class, labeller = label_both)
#' p1 + facet_grid(cyl ~ class, labeller = labeller(cyl = label_both))
#'
#' ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point() +
#'   facet_grid(vs + am ~ gear, margins=TRUE,
#'     labeller = labeller(vs = label_both, am = label_both))
#'
#'
#' capitalize <- function(string) {
#'   substr(string, 1, 1) <- toupper(substr(string, 1, 1))
#'   string
#' }
#' conservation_status <- c('cd' = 'Conservation Dependent',
#'                          'en' = 'Endangered',
#'                          'lc' = 'Least concern',
#'                          'nt' = 'Near Threatened',
#'                          'vu' = 'Vulnerable',
#'                          'domesticated' = 'Domesticated')
#' ## Source: http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#'
#' p2 <- ggplot(msleep, aes(x = sleep_total, y = awake)) + geom_point()
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(vore = capitalize))
#'
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(
#'   vore = capitalize,
#'   conservation = conservation_status
#' ))
#'
#' # We could of course have renamed the levels;
#' # then we can apply another nifty function
#' msleep$conservation2 <- plyr::revalue(msleep$conservation, conservation_status)
#'
#' p2 %+% msleep +
#'   facet_grid(vore ~ conservation2, labeller = labeller(vore = capitalize))
#' p2 %+% msleep +
#'   facet_grid(vore ~ conservation2,
#'     labeller = labeller(conservation2 = label_wrap_gen(10))
#'   )
#' }
#'
#' # When multiple factors index the facetting, simply refer to one of
#' # them to assign a labeller:
#' p3 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p3 + facet_grid(gear + am ~ vs + cyl, labeller = labeller(
#'   vs = label_bquote(.(cyl) ^ .(vs)),
#'   .default = label_value
#' ))
labeller <- function(..., keep.as.numeric = NULL, .as_character = FALSE,
                     .multi_line = TRUE, .default = label_value) {
  if (!is.null(keep.as.numeric)) {
    .Deprecated(".as_character", old = "keep.as.numeric")
  }
  args <- list(...)

  function(labels) {
    variable <- names(labels)
    labels <- lapply(labels, function(values) {
      if (is.logical(values)) {
        as.integer(values) + 1
      }
      if (is.factor(values)) {
        as.character(values)
      } else if (is.numeric(values) && .as_character) {
        as.character(values)
      } else {
        values
      }
    })

    labeller <- args[variable][[1]]

    # If the facetting margin (i.e. `variable`) was not specified when
    # calling labeller, default to use the actual values.
    if (is.null(labeller)) {
      .default(labels)

    } else if (is_labeller(labeller)) {
      if ("multi_line" %in% names(formals(labeller))) {
        labeller(labels = labels, multi_line = .multi_line)
      } else {
        labeller(labels = labels)
      }

    } else if (is.function(labeller)) {
      .default(lapply(labels, labeller), multi_line = .multi_line)

    } else {
      .default(lapply(labels, function(x) labeller[x]), multi_line = .multi_line)
    }
  }
}


build_strip <- function(panel, label_df, labeller, theme, side = "right", switch = NULL) {
  side <- match.arg(side, c("top", "left", "bottom", "right"))
  horizontal <- side %in% c("top", "bottom")
  labeller <- match.fun(labeller)

  # No labelling data, so return empty row/col
  if (empty(label_df)) {
    if (horizontal) {
      widths <- unit(rep(0, max(panel$layout$COL)), "null")
      return(gtable_row_spacer(widths))
    } else {
      heights <- unit(rep(0, max(panel$layout$ROW)), "null")
      return(gtable_col_spacer(heights))
    }
  }

  # Create matrix of labels
  labels <- lapply(labeller(label_df), cbind)
  labels <- do.call("cbind", labels)

  # Display the mirror of the y strip labels if switched
  if (!is.null(switch) && switch %in% c("both", "y")) {
    theme$strip.text.y$angle <- adjust_angle(theme$strip.text.y$angle)
  }

  # Render as grobs
  grobs <- apply(labels, c(1, 2), ggstrip, theme = theme,
    horizontal = horizontal)

  # Create layout
  name <- paste("strip", side, sep = "-")
  if (horizontal) {
    # Each row is as high as the highest and as a wide as the panel
    row_height <- function(row) max(plyr::laply(row, height_cm))
    grobs <- t(grobs)
    heights <- unit(apply(grobs, 1, row_height), "cm")
    widths <- unit(rep(1, ncol(grobs)), "null")
  } else {
    # Each row is wide as the widest and as high as the panel
    col_width <- function(col) max(plyr::laply(col, width_cm))
    widths <- unit(apply(grobs, 2, col_width), "cm")
    heights <- unit(rep(1, nrow(grobs)), "null")
  }
  gtable_matrix(name, grobs, heights = heights, widths = widths)
}

# Grob for strip labels
ggstrip <- function(text, horizontal = TRUE, theme) {
  text_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  label <- element_render(theme, text_theme, text, expand_x = !horizontal,
    expand_y = horizontal)

  ggname("strip", absoluteGrob(
    gList(
      element_render(theme, "strip.background"),
      label
    ),
    width = grobWidth(label),
    height = grobHeight(label)
  ))
}

# Helper to adjust angle of switched strips
adjust_angle <- function(angle) {
  if (is.null(angle)) {
    -90
  } else if ((angle + 180) > 360) {
    angle - 180
  } else {
    angle + 180
  }
}
