#' Get a summary of the layers in a built plot object
#'
#'
#' @export
summarise_layers <- function(p) {
  stopifnot(inherits(p, "ggplot_built"))

  # Default mappings. Make sure it's a regular list instead of an uneval
  # object.
  default_mapping <- unclass(p$plot$mapping)

  layer_mappings <- lapply(p$plot$layers, function(layer) {
    defaults(layer$mapping, default_mapping)
  })


  tibble(
    mapping = layer_mappings
  )
}
