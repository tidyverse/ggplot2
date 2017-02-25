#' Add components to a plot
#'
#' \code{+} is the key to constructing sophisticated ggplot2 graphics. It
#' allows you to start simple, then get more and more complex, checking your
#' work at each step.
#'
#' @section What can you add?:
#' You can add any of the following types of objects:
#'
#' \itemize{
#'   \item A \code{\link{aes}()} objects replaces the default aesthetics.
#'   \item A layer created by a \code{geom_} or \code{stat_} function adds
#'     new layer.
#'   \item A \code{scale} overrides the existing scale.
#'   \item A \code{\link{theme}} modifies the current theme.
#'   \item A \code{coord} overrides current coordinate system.
#'   \item A \code{facet} specificatio override current faceting.
#' }
#'
#' To replace the current default data frame, you must use \code{\%+\%},
#' due to S3 method precedence issues.
#'
#' You can also supply a list, in which case each element of the list will
#' be added in turn.
#'
#' @param e1 An object of class \code{\link{ggplot}} or a \code{\link{theme}}.
#' @param e2 A plot component, as described below.
#' @seealso \code{\link{theme}}
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
         " Did you forget to add this object to a ggplot object?")
  }
}


#' @rdname gg-add
#' @export
"%+%" <- `+.gg`

add_ggplot <- function(p, object, objectname) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (is.theme(object)) {
    p$theme <- update_theme(p$theme, object)
  } else if (inherits(object, "Scale")) {
    p$scales$add(object)
  } else if (inherits(object, "labels")) {
    p <- update_labels(p, object)
  } else if (inherits(object, "guides")) {
    p <- update_guides(p, object)
  } else if (inherits(object, "uneval")) {
      p$mapping <- defaults(object, p$mapping)
      # defaults() doesn't copy class, so copy it.
      class(p$mapping) <- class(object)

      labels <- lapply(object, deparse)
      names(labels) <- names(object)
      p <- update_labels(p, labels)
  } else if (is.Coord(object)) {
      p$coordinates <- object
      p
  } else if (is.facet(object)) {
      p$facet <- object
      p
  } else if (is.list(object)) {
    for (o in object) {
      p <- p %+% o
    }
  } else if (is.layer(object)) {
    p$layers <- append(p$layers, object)

    # Add any new labels
    mapping <- make_labels(object$mapping)
    default <- make_labels(object$stat$default_aes)
    new_labels <- defaults(mapping, default)
    p$labels <- defaults(p$labels, new_labels)
  } else {
    stop("Don't know how to add ", objectname, " to a plot",
      call. = FALSE)
  }
  set_last_plot(p)
  p
}
