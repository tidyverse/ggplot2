#' Add a new component to a ggplot or theme object.
#'
#' This operator allows you to add objects to a ggplot or theme object.
#'
#' @section Adding on to a ggplot object:
#' You can add any of the following types of objects:
#'
#' \itemize{
#'   \item \code{uneval}: replace current aesthetics
#'   \item \code{layer}: add new layer
#'   \item \code{theme}: update plot theme
#'   \item \code{scale}: replace current scale
#'   \item \code{coord}: override current coordinate system
#'   \item \code{facet}: override current coordinate faceting
#'   \item \code{list}: a list of any of the above.
#' }
#'
#' To replace the current default data frame, you must use \code{\%+\%},
#' due to S3 method precedence issues.
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
#' @param e1 An object of class \code{\link{ggplot}} or a \code{\link{theme}}.
#' @param e2 A component, or list of components to add to \code{e1}.
#' @seealso \code{\link{theme}}
#' @export
#' @method + gg
#' @rdname gg-add
#' @examples
#' # Adding on to a plot -----------------------------------------------
#'
#' base <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#' base + geom_smooth()
#'
#' # To override the data, you must use %+%
#' base %+% subset(mpg, fl == "p")
#'
#' # Alternatively, you can add multiple components with a list.
#' # This can be useful to return from a list.
#' base + list(subset(mpg, fl == "p"), geom_smooth())
#'
#' # Adding on to a theme ----------------------------------------------
#'
#' # Compare these results of adding theme objects to other theme objects
#' add_el <- theme_grey() + theme(text = element_text(family = "Times"))
#' rep_el <- theme_grey() %+replace% theme(text = element_text(family = "Times"))
#'
#' add_el$text
#' rep_el$text
"+.gg" <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is.theme(e1))  add_theme(e1, e2, e2name)
  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
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
