#' Add components to a plot
#'
#' `+` is the key to constructing sophisticated ggplot2 graphics. It
#' allows you to start simple, then get more and more complex, checking your
#' work at each step.
#'
#' @section What can you add?:
#' You can add any of the following types of objects:
#'
#'   - An [aes()] object replaces the default aesthetics.
#'   - A layer created by a `geom_` or `stat_` function adds a
#'     new layer.
#'   - A `scale` overrides the existing scale.
#'   - A [theme()] modifies the current theme.
#'   - A `coord` overrides the current coordinate system.
#'   - A `facet` specification overrides the current faceting.
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
  if (missing(e2)) {
    stop("Cannot use `+.gg()` with a single argument. ",
         "Did you accidentally put + on a new line?",
         call. = FALSE)
  }

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
#' @param plot The ggplot object to add `object` to
#' @param object_name The name of the object to add
#'
#' @return A modified ggplot object
#'
#' @keywords internal
#' @export
ggplot_add <- function(object, plot, object_name) {
  UseMethod("ggplot_add")
}
#' @export
ggplot_add.default <- function(object, plot, object_name) {
  stop("Don't know how to add ", object_name, " to a plot", call. = FALSE)
}
#' @export
ggplot_add.NULL <- function(object, plot, object_name) {
  plot
}
#' @export
ggplot_add.data.frame <- function(object, plot, object_name) {
  plot$data <- object
  plot
}
#' @export
ggplot_add.theme <- function(object, plot, object_name) {
  plot$theme <- update_theme(plot$theme, object)
  plot
}
#' @export
ggplot_add.Scale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot
}
#' @export
ggplot_add.labels <- function(object, plot, object_name) {
  update_labels(plot, object)
}
#' @export
ggplot_add.guides <- function(object, plot, object_name) {
  update_guides(plot, object)
}
#' @export
ggplot_add.uneval <- function(object, plot, object_name) {
  plot$mapping <- defaults(object, plot$mapping)
  # defaults() doesn't copy class, so copy it.
  class(plot$mapping) <- class(object)

  labels <- lapply(object, function(x) if (is.null(x)) x else rlang::quo_name(x))
  names(labels) <- names(object)
  update_labels(plot, labels)
}
#' @export
ggplot_add.Coord <- function(object, plot, object_name) {
  if (!isTRUE(plot$coordinates$default)) {
    message(
      "Coordinate system already present. Adding new coordinate ",
      "system, which will replace the existing one."
    )
  }

  plot$coordinates <- object
  plot
}
#' @export
ggplot_add.Facet <- function(object, plot, object_name) {
  plot$facet <- object
  plot
}
#' @export
ggplot_add.list <- function(object, plot, object_name) {
  for (o in object) {
    plot <- plot %+% o
  }
  plot
}
#' @export
ggplot_add.Layer <- function(object, plot, object_name) {
  plot$layers <- append(plot$layers, object)

  # Add any new labels
  mapping <- make_labels(object$mapping)
  default <- make_labels(object$stat$default_aes)
  new_labels <- defaults(mapping, default)
  plot$labels <- defaults(plot$labels, new_labels)
  plot
}
