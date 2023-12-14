#' Useful labeller functions
#'
#' Labeller functions are in charge of formatting the strip labels of
#' facet grids and wraps. Most of them accept a `multi_line`
#' argument to control whether multiple factors (defined in formulae
#' such as `~first + second`) should be displayed on a single
#' line separated with commas, or each on their own line.
#'
#' `label_value()` only displays the value of a factor while
#' `label_both()` displays both the variable name and the factor
#' value. `label_context()` is context-dependent and uses
#' `label_value()` for single factor faceting and
#' `label_both()` when multiple factors are
#' involved. `label_wrap_gen()` uses [base::strwrap()]
#' for line wrapping.
#'
#' `label_parsed()` interprets the labels as plotmath
#' expressions. [label_bquote()] offers a more flexible
#' way of constructing plotmath expressions. See examples and
#' [bquote()] for details on the syntax of the
#' argument.
#'
#' @section Writing New Labeller Functions:
#'
#'   Note that an easy way to write a labeller function is to
#'   transform a function operating on character vectors with
#'   [as_labeller()].
#'
#'   A labeller function accepts a data frame of labels (character
#'   vectors) containing one column for each factor. Multiple factors
#'   occur with formula of the type `~first + second`.
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
#'   If it's useful to your labeller, you can retrieve the `type`
#'   attribute of the incoming data frame of labels. The value of this
#'   attribute reflects the kind of strips your labeller is dealing
#'   with: `"cols"` for columns and `"rows"` for rows. Note
#'   that [facet_wrap()] has columns by default and rows
#'   when the strips are switched with the `switch` option. The
#'   `facet` attribute also provides metadata on the labels. It
#'   takes the values `"grid"` or `"wrap"`.
#'
#'   For compatibility with [labeller()], each labeller
#'   function must have the `labeller` S3 class.
#'
#' @param labels Data frame of labels. Usually contains only one
#'   element, but faceting over multiple factors entails multiple
#'   label variables.
#' @param multi_line Whether to display the labels of multiple factors
#'   on separate lines.
#' @param sep String separating variables and values.
#' @param width Maximum number of characters before wrapping the strip.
#' @family facet
#' @seealso [labeller()], [as_labeller()],
#'   [label_bquote()]
#' @examples
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # The default is label_value
#' p + facet_grid(. ~ cyl, labeller = label_value)
#'
#' \donttest{
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
#' }
#' @name labellers
NULL

collapse_labels_lines <- function(labels) {
  is_exp <- vapply(labels, function(l) length(l) > 0 && is.expression(l[[1]]), logical(1))
  out <- inject(mapply(paste, !!!labels, sep = ", ", SIMPLIFY = FALSE))
  label <- list(unname(unlist(out)))
  if (all(is_exp)) {
    label <- lapply(label, function(l) list(parse(text = paste0("list(", l, ")"))))
  }
  label
}

#' @rdname labellers
#' @export
label_value <- function(labels, multi_line = TRUE) {
  labels <- lapply(labels, as.character)
  if (multi_line) {
    labels
  } else {
    collapse_labels_lines(labels)
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
    value <- inject(paste(!!!value, sep = ", "))
    variable <- inject(paste(!!!variable, sep = ", "))
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

find_names <- function(expr) {
  if (is.call(expr)) {
    unlist(lapply(expr[-1], find_names))
  } else if (is.name(expr)) {
    as.character(expr)
  }
}

#' Label with mathematical expressions
#'
#' `label_bquote()` offers a flexible way of labelling
#' facet rows or columns with plotmath expressions. Backquoted
#' variables will be replaced with their value in the facet.
#'
#' @param rows Backquoted labelling expression for rows.
#' @param cols Backquoted labelling expression for columns.
#' @param default Unused, kept for compatibility.
#' @seealso \link{labellers}, [labeller()],
#' @export
#' @examples
#' # The variables mentioned in the plotmath expression must be
#' # backquoted and referred to by their names.
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + facet_grid(vs ~ ., labeller = label_bquote(alpha ^ .(vs)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(cols = .(vs) ^ .(vs)))
#' p + facet_grid(. ~ vs + am, labeller = label_bquote(cols = .(am) ^ .(vs)))
label_bquote <- function(rows = NULL, cols = NULL,
                         default) {
  cols_quoted <- substitute(cols)
  rows_quoted <- substitute(rows)

  call_env <- env_parent()

  fun <- function(labels) {
    quoted <- resolve_labeller(rows_quoted, cols_quoted, labels)
    if (is.null(quoted)) {
      return(label_value(labels))
    }

    evaluate <- function(...) {
      params <- list(...)
      params <- as_environment(params, call_env)
      eval(substitute(bquote(expr, params), list(expr = quoted)))
    }
    list(inject(mapply(evaluate, !!!labels, SIMPLIFY = FALSE)))
  }

  structure(fun, class = "labeller")
}
utils::globalVariables(c("x", "."))

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

resolve_labeller <- function(rows, cols, labels) {
  if (is.null(cols) && is.null(rows)) {
    cli::cli_abort("Supply one of {.arg rows} or {.arg cols}.")
  }
  if (attr(labels, "facet") == "wrap") {
    # Return either rows or cols for facet_wrap()
    if (!is.null(cols) && !is.null(rows)) {
      cli::cli_abort("Cannot supply both {.arg rows} and {.arg cols} to {.fn facet_wrap}.")
    }
    cols %||% rows
  } else {
    if (attr(labels, "type") == "rows") {
      rows
    } else {
      cols
    }
  }
}

#' Coerce to labeller function
#'
#' This transforms objects to labeller functions. Used internally by
#' [labeller()].
#' @param x Object to coerce to a labeller function. If a named
#'   character vector, it is used as a lookup table before being
#'   passed on to `default`. If a non-labeller function, it is
#'   assumed it takes and returns character vectors and is applied to
#'   the labels. If a labeller, it is simply applied to the labels.
#' @param multi_line Whether to display the labels of multiple factors
#'   on separate lines. This is passed to the labeller function.
#' @param default Default labeller to process the labels produced by
#'   lookup tables or modified by non-labeller functions.
#' @seealso [labeller()], \link{labellers}
#' @keywords internal
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(disp, drat)) + geom_point()
#' p + facet_wrap(~am)
#'
#' # Rename labels on the fly with a lookup character vector
#' to_string <- as_labeller(c(`0` = "Zero", `1` = "One"))
#' p + facet_wrap(~am, labeller = to_string)
#'
#' # Quickly transform a function operating on character vectors to a
#' # labeller function:
#' appender <- function(string, suffix = "-foo") paste0(string, suffix)
#' p + facet_wrap(~am, labeller = as_labeller(appender))
#'
#' # If you have more than one faceting variable, be sure to dispatch
#' # your labeller to the right variable with labeller()
#' p + facet_grid(cyl ~ am, labeller = labeller(am = to_string))
as_labeller <- function(x, default = label_value, multi_line = TRUE) {
  force(x)
  fun <- function(labels) {
    labels <- lapply(labels, as.character)

    # Dispatch multi_line argument to the labeller function instead of
    # supplying it to the labeller call because some labellers do not
    # support it.
    default <- dispatch_args(default, multi_line = multi_line)

    if (is_labeller(x)) {
      x <- dispatch_args(x, multi_line = multi_line)
      x(labels)
    } else if (is.function(x)) {
      default(lapply(labels, x))
    } else if (is.formula(x)) {
      default(lapply(labels, as_function(x)))
    } else if (is.character(x)) {
      default(lapply(labels, function(label) x[label]))
    } else {
      default(labels)
    }
  }
  structure(fun, class = "labeller")
}

#' Construct labelling specification
#'
#' This function makes it easy to assign different labellers to
#' different factors. The labeller can be a function or it can be a
#' named character vectors that will serve as a lookup table.
#'
#' In case of functions, if the labeller has class `labeller`, it
#' is directly applied on the data frame of labels. Otherwise, it is
#' applied to the columns of the data frame of labels. The data frame
#' is then processed with the function specified in the
#' `.default` argument. This is intended to be used with
#' functions taking a character vector such as
#' [Hmisc::capitalize()].
#'
#' @param ... Named arguments of the form \code{variable =
#'   labeller}. Each labeller is passed to [as_labeller()]
#'   and can be a lookup table, a function taking and returning
#'   character vectors, or simply a labeller function.
#' @param .rows,.cols Labeller for a whole margin (either the rows or
#'   the columns). It is passed to [as_labeller()]. When a
#'   margin-wide labeller is set, make sure you don't mention in
#'   `...` any variable belonging to the margin.
#' @param keep.as.numeric `r lifecycle::badge("deprecated")` All supplied
#'   labellers and on-labeller functions should be able to work with character
#'   labels.
#' @param .multi_line Whether to display the labels of multiple
#'   factors on separate lines. This is passed to the labeller
#'   function.
#' @param .default Default labeller for variables not specified. Also
#'   used with lookup tables or non-labeller functions.
#' @family facet labeller
#' @seealso [as_labeller()], \link{labellers}
#' @return A labeller function to supply to [facet_grid()] or [facet_wrap()]
#'   for the argument `labeller`.
#' @export
#' @examples
#' \donttest{
#' p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#'
#' # You can assign different labellers to variables:
#' p1 + facet_grid(
#'   vs + am ~ gear,
#'   labeller = labeller(vs = label_both, am = label_value)
#' )
#'
#' # Or whole margins:
#' p1 + facet_grid(
#'   vs + am ~ gear,
#'   labeller = labeller(.rows = label_both, .cols = label_value)
#' )
#'
#' # You can supply functions operating on strings:
#' capitalize <- function(string) {
#'   substr(string, 1, 1) <- toupper(substr(string, 1, 1))
#'   string
#' }
#' p2 <- ggplot(msleep, aes(x = sleep_total, y = awake)) + geom_point()
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(vore = capitalize))
#'
#' # Or use character vectors as lookup tables:
#' conservation_status <- c(
#'   cd = "Conservation Dependent",
#'   en = "Endangered",
#'   lc = "Least concern",
#'   nt = "Near Threatened",
#'   vu = "Vulnerable",
#'   domesticated = "Domesticated"
#' )
#' ## Source: http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#'
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(
#'   .default = capitalize,
#'   conservation = conservation_status
#' ))
#'
#' # In the following example, we rename the levels to the long form,
#' # then apply a wrap labeller to the columns to prevent cropped text
#' idx <- match(msleep$conservation, names(conservation_status))
#' msleep$conservation2 <- conservation_status[idx]
#'
#' p3 <- ggplot(msleep, aes(x = sleep_total, y = awake)) + geom_point()
#' p3 +
#'   facet_grid(vore ~ conservation2,
#'     labeller = labeller(conservation2 = label_wrap_gen(10))
#'   )
#'
#' # labeller() is especially useful to act as a global labeller. You
#' # can set it up once and use it on a range of different plots with
#' # different facet specifications.
#'
#' global_labeller <- labeller(
#'   vore = capitalize,
#'   conservation = conservation_status,
#'   conservation2 = label_wrap_gen(10),
#'   .default = label_both
#' )
#'
#' p2 + facet_grid(vore ~ conservation, labeller = global_labeller)
#' p3 + facet_wrap(~conservation2, labeller = global_labeller)
#' }
labeller <- function(..., .rows = NULL, .cols = NULL,
                     keep.as.numeric = deprecated(), .multi_line = TRUE,
                     .default = label_value) {
  if (lifecycle::is_present(keep.as.numeric)) {
    deprecate_warn0("2.0.0", "labeller(keep.as.numeric)")
  }
  dots <- list2(...)
  .default <- as_labeller(.default)

  function(labels) {
    if (!is.null(.rows) || !is.null(.cols)) {
      margin_labeller <- resolve_labeller(.rows, .cols, labels)
    } else {
      margin_labeller <- NULL
    }

    if (is.null(margin_labeller)) {
      labellers <- lapply(dots, as_labeller, default = .default)
    } else {
      margin_labeller <- as_labeller(margin_labeller, default = .default,
                                     multi_line = .multi_line)

      # Check that variable-specific labellers do not overlap with
      # margin-wide labeller
      if (any(names(dots) %in% names(labels))) {
        cli::cli_abort("Conflict between {.var {paste0('.', attr(labels, 'type'))}} and {.var {names(dots)}}.")
      }
    }

    # Apply relevant labeller
    if (is.null(margin_labeller)) {
      # Apply named labeller one by one
      out <- lapply(names(labels), function(label) {
        if (label %in% names(labellers)) {
          # Yield custom labels with any NA replaced with original
          lbls <- labellers[[label]](labels[label])[[1]]
          ind <- which(is.na(lbls))
          lbls[ind] <- .default(labels[label])[[1]][ind]
          lbls
        } else {
          .default(labels[label])[[1]]
        }
      })
      names(out) <- names(labels)
      if (.multi_line) {
        out
      } else {
        collapse_labels_lines(out)
      }
    } else {
      margin_labeller(labels)
    }
  }
}

#' Build facet strips
#'
#' Builds a set of facet strips from a data frame of labels.
#'
#' @param label_df Data frame of labels to place in strips.
#' @param labeller Labelling function.
#' @param theme A theme object.
#' @param horizontal Whether the strips are horizontal (e.g. x facets) or not.
#'
#' @noRd
build_strip <- function(label_df, labeller, theme, horizontal) {
  labeller <- match.fun(labeller)

  # No labelling data, so return empty row/col
  if (empty(label_df)) {
    return(if (horizontal) {
      list(top = NULL, bottom = NULL)
    } else {
      list(left = NULL, right = NULL)
    })
  }

  # Create labels
  labels <- data_frame0(!!!labeller(label_df))
  ncol <- ncol(labels)
  nrow <- nrow(labels)
  labels_vec <- unlist(labels, use.names = FALSE)

  # Decide strip clipping
  clip <- calc_element("strip.clip", theme)[[1]]
  clip <- pmatch(clip, c("on", "off", "inherit"), nomatch = 3)
  clip <- c("on", "off", "inherit")[clip]

  if (horizontal) {
    grobs_top <- lapply(labels_vec, element_render, theme = theme,
                        element = "strip.text.x.top", margin_x = TRUE,
                        margin_y = TRUE)
    grobs_top <- assemble_strips(matrix(grobs_top, ncol = ncol, nrow = nrow),
                                 theme, horizontal, clip = clip)

    grobs_bottom <- lapply(labels_vec, element_render, theme = theme,
                           element = "strip.text.x.bottom", margin_x = TRUE,
                           margin_y = TRUE)
    grobs_bottom <- assemble_strips(matrix(grobs_bottom, ncol = ncol, nrow = nrow),
                                    theme, horizontal, clip = clip)

    list(
      top = grobs_top,
      bottom = grobs_bottom
    )
  } else {
    grobs_left <- lapply(labels_vec, element_render, theme = theme,
                         element = "strip.text.y.left", margin_x = TRUE,
                         margin_y = TRUE)
    grobs_left <- assemble_strips(matrix(grobs_left, ncol = ncol, nrow = nrow),
                                  theme, horizontal, clip = clip)

    grobs_right <- lapply(unlist(labels[, rev(seq_len(ncol(labels))), drop = FALSE], use.names = FALSE),
                          element_render, theme = theme,
                          element = "strip.text.y.right", margin_x = TRUE,
                          margin_y = TRUE)
    grobs_right <- assemble_strips(matrix(grobs_right, ncol = ncol, nrow = nrow),
                                   theme, horizontal, clip = clip)

    list(
      left = grobs_left,
      right = grobs_right
    )
  }
}

#' Grob for strip labels
#'
#' Takes the output from title_spec, adds margins, creates gList with strip
#' background and label, and returns gtable matrix.
#'
#' @param grobs Output from [titleGrob()].
#' @param theme Theme object.
#' @param horizontal Whether the strips are horizontal (e.g. x facets) or not.
#' @param clip should drawing be clipped to the specified cells (‘"on"’),the
#'   entire table (‘"inherit"’), or not at all (‘"off"’).
#'
#' @noRd
assemble_strips <- function(grobs, theme, horizontal = TRUE, clip) {
  if (length(grobs) == 0 || is.zero(grobs[[1]])) {
    # Subsets matrix of zeroGrobs to correct length (#4050)
    grobs <- grobs[seq_len(NROW(grobs))]
    return(grobs)
  }

  if (horizontal) {
    height <- max_height(grobs)
    width <- unit(1, "null")
  } else {
    height <- unit(1, "null")
    width <- max_width(grobs)
  }

  background <- if (horizontal) "strip.background.x" else "strip.background.y"
  background <- element_render(theme, background)

  # Put text on a strip
  grobs[] <- lapply(grobs, function(x) {
    ggname("strip", gTree(children = gList(background, x)))
  })
  apply(grobs, 1, function(x) {
    if (horizontal) {
      mat <- matrix(x, ncol = 1)
    } else {
      mat <- matrix(x, nrow = 1)
    }
    gtable_matrix("strip", mat, rep(width, ncol(mat)), rep(height, nrow(mat)), clip = clip)
  })
}

# Check for old school labeller
check_labeller <- function(labeller) {
  labeller <- match.fun(labeller)
  is_deprecated <- all(c("variable", "value") %in% names(formals(labeller)))

  if (is_deprecated) {
    old_labeller <- labeller
    labeller <- function(labels) {
      Map(old_labeller, names(labels), labels)
    }
    # TODO Update to lifecycle after next lifecycle release
    cli::cli_warn(c(
      "The {.arg labeller} API has been updated. Labellers taking {.arg variable} and {.arg value} arguments are now deprecated.",
      "i" = "See labellers documentation."
    ))
  }

  labeller
}
