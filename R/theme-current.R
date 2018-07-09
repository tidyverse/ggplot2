#' @include theme-defaults.r
#' @include theme-elements.r
ggplot_global$theme_current <- theme_gray()

#' Get, set, and modify the active theme
#'
#' The current/active theme is automatically applied to every plot you draw.
#' Use `theme_get` to get the current theme, and `theme_set` to
#' completely override it. `theme_update` and `theme_replace` are
#' shorthands for changing individual elements.
#'
#' @section Adding on to a theme:
#'
#' `+` and \code{\%+replace\%} can be used to modify elements in themes.
#'
#' `+` updates the elements of e1 that differ from elements specified (not
#' NULL) in e2. Thus this operator can be used to incrementally add or modify
#' attributes of a ggplot theme.
#'
#' In contrast, \code{\%+replace\%} replaces the entire element; any element of
#' a theme not specified in e2 will not be present in the resulting theme (i.e.
#' NULL). Thus this operator can be used to overwrite an entire theme.
#'
#' `theme_update` uses the `+` operator, so that any unspecified
#' values in the theme element will default to the values they are set in the
#' theme. `theme_replace` uses \code{\%+replace\%} to completely replace
#' the element, so any unspecified values will overwrite the current value in
#' the theme with `NULL`s.
#'
#' @param ... named list of theme settings
#' @param e1,e2 Theme and element to combine
#' @return `theme_set`, `theme_update`, and `theme_replace`
#'   invisibly return the previous theme so you can easily save it, then
#'   later restore it.
#' @seealso [+.gg()]
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' p
#'
#' # Use theme_set() to completely override the current theme.
#' # Here we have the old theme so we can later restore it.
#' # Note that the theme is applied when the plot is drawn, not
#' # when it is created.
#' old <- theme_set(theme_bw())
#' p
#' theme_set(old)
#' p
#'
#'
#' # Modifying theme objects -----------------------------------------
#' # You can use + and %+replace% to modify a theme object.
#' # They differ in how they deal with missing arguments in
#' # the theme elements.
#'
#' add_el <- theme_grey() +
#'   theme(text = element_text(family = "Times"))
#' add_el$text
#'
#' rep_el <- theme_grey() %+replace%
#'   theme(text = element_text(family = "Times"))
#' rep_el$text
#'
#' # theme_update() and theme_replace() are similar except they
#' # apply directly to the current/active theme.
theme_get <- function() {
  ggplot_global$theme_current
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

  old <- ggplot_global$theme_current
  ggplot_global$theme_current <- new
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
