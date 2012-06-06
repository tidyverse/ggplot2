#' Modify a plot by adding on new components.
#' 
#' What happens when you add on:
#'
#' \itemize{
#'   \item \code{data.frame}: replace current data.frame 
#'      (must use \code{\%+\%})
#'   \item \code{uneval}: replace current aesthetics
#'   \item \code{layer}: add new layer
#'   \item \code{options}: update plot options
#'   \item \code{scale}: replace current scale
#'   \item \code{coord}: override current coordinate system
#'   \item \code{facet}: override current coordinate faceting
#' }
#'
#' @param p plot object
#' @param object component to add
#' @seealso \code{\link{ggplot}}
#' @method + ggplot
#' @S3method "+" ggplot
#' @rdname ggplot-add
"+.ggplot" <- function(e1, e2) {
  if      (is.theme(e1))  add_theme(e1, e2)
  else if (is.ggplot(e1)) add_ggplot(e1, e2)
}
# This wrapper function is needed because S3 tries to dispatch on both
# arguments and gets confused when +.ggplot and +.theme are different.
# If they're defined to be the same, it works, but then we need to manually
# dispatch based on the type of the objects.


#' Modify elements in a theme object
#'
#' The \code{+} operator and the \code{\%+replace\%} can be used to modify
#' elements in themes.
#'
#' The \code{+} operator completely replaces elements
#' with elements from e2.
#'
#' In contrast, the \code{\%+replace\%} operator does not replace the
#' entire element; it only updates element properties which are present
#' (not NULL) in the second object.
#'
#' @param e1 theme object
#' @param e2 theme object to use for updating e1
#' @export
#' @examples
#' # Compare these results
#' add_el <- theme_grey() + theme(text = element_text(family = "Times"))
#' rep_el <- theme_grey() %+replace% theme(text = element_text(family = "Times"))
#'
#' add_el$text
#' rep_el$text
#'
#' @S3method "+" theme
#' @rdname theme-add
"+.theme" <- `+.ggplot`


add_ggplot <- function(p, object) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (is.theme(object)) {
    p$options <- defaults(object, p$options)
  } else if (inherits(object, "scale")) {
    p$scales$add(object)
  } else if(inherits(object, "labels")) {
    p <- update_labels(p, object)
  } else if(inherits(object, "guides")) {
    p <- update_guides(p, object)
  } else if(inherits(object, "uneval")) {
      p$mapping <- defaults(object, p$mapping)
      
      labels <- lapply(object, deparse)
      names(labels) <- names(object)
      p <- update_labels(p, labels)
  } else if (is.coord(object)) {
      p$coordinates <- object
      p
  } else if (is.facet(object)) {
      p$facet <- object
      p
  } else if(is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else if(is.proto(object)) {
    p <- switch(object$class(),
      layer  = {
        p$layers <- append(p$layers, object)
        
        # Add any new labels
        mapping <- make_labels(object$mapping)
        default <- make_labels(object$stat$default_aes())
        
        new_labels <- defaults(mapping, default)
        p$labels <- defaults(p$labels, new_labels)
        p
      },
      coord = {
        p$coordinates <- object
        p
      }
    )
  } else {
    stop("Don't know how to add ", orig_args(object), " to a plot",
      call. = FALSE)
  }
  set_last_plot(p)
  p
}

#' @rdname ggplot-add
#' @export
"%+%" <- `+.ggplot`
