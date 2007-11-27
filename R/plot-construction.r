"%+%" <- "+.ggplot" <- function(p, object) {
  p <- plot_clone(p)

  if (is.data.frame(object)) {
    p$data <- object
  } else if (inherits(object, "options")) {
    p <- do.call("update", c(list(p), object))
  } else if(inherits(object, "uneval")) {
      p$defaults <- defaults(object, p$defaults)
  } else if(is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else {
    p <- switch(object$class(),
      layer  = {
        p$layers <- append(p$layers, object)
        data <- if(is.null(object$data)) p$data else object$data
        mapping <- if(is.null(object$aesthetics)) p$defaults else object$aesthetics
        p$scales$add_defaults(data, mapping)
        p
      },
      geom = p + layer(geom = object),
      stat = p + layer(stat = object),
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
  }
  if (ggopt()$auto.print & length(p$layers) > 0) try(print(p))
  (.last_plot <<- p)
}