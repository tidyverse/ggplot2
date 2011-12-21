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
"+.ggplot" <- function(p, object) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (inherits(object, "options")) {
    object$labels <- defaults(object$labels, p$options$labels)
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
        p$options$labels <- defaults(p$options$labels, new_labels)
        p
      },
      coord = {
        p$coordinates <- object
        p
      }
    )
  } else {
    stop("Don't know how to add ", deparse(substitute(object)), " to a plot",
      call. = FALSE)
  }
  set_last_plot(p)
  p
}

#' @rdname ggplot-add
#' @export
"%+%" <- `+.ggplot`
