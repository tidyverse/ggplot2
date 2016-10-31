#' @include theme-defaults.r
#' @include theme-elements.r
theme_env <- new.env(parent = emptyenv())
theme_env$current <- theme_gray()

#' Get, set and update themes
#'
#' Use \code{theme_get} to get the current theme, and \code{theme_set} to
#' completely override it. \code{theme_update} and \code{theme_replace} are
#' shorthands for changing individual elements in the current theme.
#'
#' @section Adding on to a theme:
#'
#' \code{+} and \code{\%+replace\%} can be used to modify elements in themes.
#'
#' \code{+} updates the elements of e1 that differ from elements specified (not
#' NULL) in e2. Thus this operator can be used to incrementally add or modify
#' attributes of a ggplot theme.
#'
#' In contrast, \code{\%+replace\%} replaces the entire element; any element of
#' a theme not specified in e2 will not be present in the resulting theme (i.e.
#' NULL). Thus this operator can be used to overwrite an entire theme.
#'
#' \code{theme_update} uses the \code{+} operator, so that any unspecified
#' values in the theme element will default to the values they are set in the
#' theme. \code{theme_replace} uses \code{\%+replace\%} tocompletely replace
#' the element, so any unspecified values will overwrite the current value in
#' the theme with \code{NULL}s.
#'
#' @param ... named list of theme settings
#' @param e1,e2 Theme and element to combine
#' @seealso \code{\link{+.gg}}
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
#' # Adding on to a theme ----------------------------------------------
#'
#' # Compare these results of adding theme objects to other theme objects
#' add_el <- theme_grey() + theme(text = element_text(family = "Times"))
#' rep_el <- theme_grey() %+replace% theme(text = element_text(family = "Times"))
#'
#' add_el$text
#' rep_el$text
theme_get <- function() {
  theme_env$current
}

#' @rdname theme_get
#' @param new new theme (a list of theme elements)
#' @export
theme_set <- function(new) {
  missing <- setdiff(names(theme_gray()), names(new))
  if (length(missing) > 0) {
    warning("New theme missing the following elements: ",
      paste(missing, collapse = ", "), call. = FALSE)
  }

  old <- theme_env$current
  theme_env$current <- new
  invisible(old)
}

#' @rdname theme_get
#' @export
theme_update <- function(...) {
  theme_set(theme_get() + theme(...))
}

#' @rdname theme_get
#' @export
theme_replace <- function(...) {
  theme_set(theme_get() %+replace% theme(...))
}

#' @rdname theme_get
#' @export
"%+replace%" <- function(e1, e2) {
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }

  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}
