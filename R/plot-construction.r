# Plot construction
# The elements of a ggplot plot are combined together with addition.
# 
# \itemize{
#   \item \code{data.frame}: replace default data.frame (must use \code{\%+\%})
#   \item \code{uneval}: replace default aesthetics
#   \item \code{layer}: add new layer
#   \item \code{options}: update plot options
#   \item \code{scale}: replace default scale
#   \item \code{coord}: override default coordinate system
#   \item \code{facet}: override default coordinate faceting
# }
#
# @arguments plot object
# @arguments object to add
# @seealso \code{\link{set_last_plot}}, \code{\link{ggplot}}
# @keyword internal
# @alias \%+\%
"+.ggplot" <- function(p, object) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (inherits(object, "options")) {
    object$labels <- plyr::defaults(object$labels, p$options$labels)
    p$options <- plyr::defaults(object, p$options)
  } else if(inherits(object, "labels")) {
      p <- update_labels(p, object)
  } else if(inherits(object, "uneval")) {
      p$mapping <- plyr::defaults(object, p$mapping)
      
      labels <- lapply(object, deparse)
      names(labels) <- names(object)
      p <- update_labels(p, labels)
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
        
        new_labels <- plyr::defaults(mapping, default)
        p$options$labels <- plyr::defaults(p$options$labels, new_labels)
        p
      },
      coord = {
        p$coordinates <- object
        p
      },
      facet = {
        p$facet <- object
        p
      },
      scale = {
        p$scales$add(object)
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
"%+%" <- `+.ggplot`