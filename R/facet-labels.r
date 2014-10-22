#' Label facets with their value.
#' This is the default labelling scheme.
#'
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(. ~ cyl, labeller = label_value)
label_value <- function(variable, value) as.character(value)

#' Label facets with value and variable.
#'
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(. ~ cyl, labeller = label_both)
label_both <- function(variable, value) paste(variable, value, sep = ": ")

#' Label facets with parsed label.
#'
#' @seealso \code{\link{plotmath}}
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
#' qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2,
#'   labeller = label_parsed)
label_parsed <- function(variable, value) {
  llply(as.character(value), function(x) parse(text = x))
}

#' Label facet with 'bquoted' expressions
#'
#' See \code{\link{bquote}} for details on the syntax of the argument.  The
#' label value is x.
#'
#' @param expr labelling expression to use
#' @family facet labellers
#' @seealso \code{\link{plotmath}}
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(.(x) ^ .(x)))
label_bquote <- function(expr = beta ^ .(x)) {
  quoted <- substitute(expr)

  function(variable, value) {
    value <- as.character(value)
    lapply(value, function(x)
      eval(substitute(bquote(expr, list(x = x)), list(expr = quoted))))
  }
}
globalVariables("x")


#' Label facets with a word wrapped label.
#'
#' Uses \code{\link[base]{strwrap}} for line wrapping.
#' @param width integer, target column width for output.
#' @export
#' @seealso , \code{\link{labeller}}
label_wrap_gen <- function(width = 25) {
  function(variable, values) {
    vapply(strwrap(as.character(values), width = width, simplify = FALSE),
           paste, vector('character', 1), collapse = "\n")
  }
}

#' Generic labeller function for facets
#'
#' One-step function for providing methods or named character vectors
#' for displaying labels in facets.
#'
#' The provided methods are checked for number of arguments.
#' If the provided method takes less than two
#' (e.g. \code{\link[Hmisc]{capitalize}}),
#' the method is passed \code{values}.
#' Else (e.g. \code{\link{label_both}}),
#' it is passed \code{variable} and \code{values} (in that order).
#' If you want to be certain, use e.g. an anonymous function.
#' If errors are returned such as ``argument ".." is missing, with no default''
#' or ``unused argument (variable)'', matching the method's arguments does not
#' work as expected; make a wrapper function.
#'
#'
#' @param ... Named arguments of the form \code{variable=values},
#'   where \code{values} could be a vector or method.
#' @param keep.as.numeric logical, default TRUE. When FALSE, converts numeric
#'   values supplied as margins to the facet to characters.
#' @family facet labeller
#' @return Function to supply to
#'   \code{\link{facet_grid}} for the argument \code{labeller}.
#' @export
#' @examples
#' \donttest{
#' p1 <- ggplot(mpg, aes(cty, hwy)) + geom_point()
#' p1 + facet_grid(cyl ~ class, labeller=label_both)
#' p1 + facet_grid(cyl ~ class, labeller=labeller(cyl=label_both))
#'
#' ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point() +
#'   facet_grid(vs + am ~ gear, margins=TRUE,
#'              labeller=labeller(vs=label_both, am=label_both))
#'
#' capitalize <- function(string) {
#'   substr(string, 1, 1) <- toupper(substr(string, 1, 1))
#'   string
#' }
#' conservation_status <- c('cd'='Conservation Dependent',
#'                          'en'='Endangered',
#'                          'lc'='Least concern',
#'                          'nt'='Near Threatened',
#'                          'vu'='Vulnerable',
#'                          'domesticated'='Domesticated')
#' ## Source: http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#'
#' p2 <- ggplot(msleep, aes(x=sleep_total, y=awake)) + geom_point()
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(vore = capitalize))
#'
#' p2 + facet_grid(vore ~ conservation,
#'   labeller=labeller(vore = capitalize, conservation = conservation_status ))
#'
#' # We could of course have renamed the levels;
#' # then we can apply another nifty function
#' msleep$conservation2 <- plyr::revalue(msleep$conservation, conservation_status)
#'
#' p2 %+% msleep +
#'   facet_grid(vore ~ conservation2, labeller = labeller(vore = capitalize))
#' p2 %+% msleep +
#'  facet_grid(vore ~ conservation2, labeller = labeller(conservation2 =
#'  label_wrap_gen(10)))
#' }
labeller <- function(..., keep.as.numeric=FALSE) {
  args <- list(...)

  function(variable, values) {
    if (is.logical(values)) {
      values <- as.integer(values) + 1
    } else if (is.factor(values)) {
      values <- as.character(values)
    } else if (is.numeric(values) & !keep.as.numeric) {
      values <- as.character(values)
    }

    res <- args[[variable]]

    if (is.null(res)) {
      # If the facetting margin (i.e. `variable`) was not specified when calling
      # labeller, default to use the actual values.
      result <- values

    } else if (is.function(res)) {
      # How should `variable` and `values` be passed to a function? ------------
      arguments <- length(formals(res))
      if (arguments < 2) {
        result <- res(values)
      } else {
        result <- res(variable, values)
      }

    } else {
      result <- res[values]
    }

    return(result)
  }
}



# Grob for strip labels
ggstrip <- function(text, horizontal=TRUE, theme) {
  text_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  label <- element_render(theme, text_theme, text)

  ggname("strip", absoluteGrob(
    gList(
      element_render(theme, "strip.background"),
      label
    ),
    width = grobWidth(label) + unit(0.5, "lines"),
    height = grobHeight(label) + unit(0.5, "lines")
  ))
}
