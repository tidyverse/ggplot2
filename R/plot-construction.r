#' Add components to a plot
#'
#' `+` is the key to constructing sophisticated ggplot2 graphics. It
#' allows you to start simple, then get more and more complex, checking your
#' work at each step.
#'
#' @section What can you add?:
#' You can add any of the following types of objects:
#'
#'   - A [aes()] objects replaces the default aesthetics.
#'   - A layer created by a `geom_` or `stat_` function adds
#'     new layer.
#'   - A `scale` overrides the existing scale.
#'   - A [theme()] modifies the current theme.
#'   - A `coord` overrides current coordinate system.
#'   - A `facet` specificatio override current faceting.
#'
#' To replace the current default data frame, you must use \code{\%+\%},
#' due to S3 method precedence issues.
#'
#' You can also supply a list, in which case each element of the list will
#' be added in turn.
#'
#' @param e1 An object of class [ggplot()] or a [theme()].
#' @param e2 A plot component, as described below.
#' @seealso [theme()]
#' @export
#' @method + gg
#' @rdname gg-add
#' @examples
#' base <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#' base + geom_smooth()
#'
#' # To override the data, you must use %+%
#' base %+% subset(mpg, fl == "p")
#'
#' # Alternatively, you can add multiple components with a list.
#' # This can be useful to return from a function.
#' base + list(subset(mpg, fl == "p"), geom_smooth())
"+.gg" <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is.theme(e1))  add_theme(e1, e2, e2name)
  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
  else if (is.ggproto(e1)) {
    stop("Cannot add ggproto objects together.",
         " Did you forget to add this object to a ggplot object?",
         call. = FALSE)
  }
}


#' @rdname gg-add
#' @export
"%+%" <- `+.gg`

add_ggplot <- function(p, object, objectname) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  p <- ggplot_add(object, p, objectname)
  set_last_plot(p)
  p
}
#' Add custom objects to ggplot
#'
#' This generic allows you to add your own methods for adding custom objects to
#' a ggplot with [+.gg].
#'
#' @param object An object to add to the plot
#' @param p The ggplot object to add `object` to
#' @param objectname The name of the object to add
#'
#' @return A modified ggplot object
#'
#' @keywords internal
#' @export
ggplot_add <- function(object, p, objectname) {
  UseMethod("ggplot_add")
}
#' @export
ggplot_add.default <- function(object, p, objectname) {
  stop("Don't know how to add ", objectname, " to a plot", call. = FALSE)
}
#' @export
ggplot_add.data.frame <- function(object, p, objectname) {
  p$data <- object
  p
}
#' @export
ggplot_add.theme <- function(object, p, objectname) {
  p$theme <- update_theme(p$theme, object)
  p
}
#' @export
ggplot_add.Scale <- function(object, p, objectname) {
  p$scales$add(object)
  p
}
#' @export
ggplot_add.labels <- function(object, p, objectname) {
  update_labels(p, object)
}
#' @export
ggplot_add.guides <- function(object, p, objectname) {
  update_guides(p, object)
}
#' @export
ggplot_add.uneval <- function(object, p, objectname) {
  p$mapping <- defaults(object, p$mapping)
  # defaults() doesn't copy class, so copy it.
  class(p$mapping) <- class(object)

  labels <- lapply(object, deparse)
  names(labels) <- names(object)
  update_labels(p, labels)
}
#' @export
ggplot_add.Coord <- function(object, p, objectname) {
  p$coordinates <- object
  p
}
#' @export
ggplot_add.Facet <- function(object, p, objectname) {
  p$facet <- object
  p
}
#' @export
ggplot_add.list <- function(object, p, objectname) {
  for (o in object) {
    p <- p %+% o
  }
  p
}
#' @export
ggplot_add.Layer <- function(object, p, objectname) {
  p$layers <- append(p$layers, object)

  # Add any new labels
  mapping <- make_labels(object$mapping)
  default <- make_labels(object$stat$default_aes)
  new_labels <- defaults(mapping, default)
  p$labels <- defaults(p$labels, new_labels)
  p
}
