#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
StatSf <- ggproto("StatSf", Stat,
  compute_group = function(data, scales) {
    bbox <- sf::st_bbox(data[[ geom_column(data) ]])
    data$xmin <- bbox[["xmin"]]
    data$xmax <- bbox[["xmax"]]
    data$ymin <- bbox[["ymin"]]
    data$ymax <- bbox[["ymax"]]

    data
  },

  required_aes = c("geometry")
)

#' @export
#' @rdname ggsf
#' @inheritParams stat_identity
stat_sf <- function(mapping = NULL, data = NULL, geom = "rect",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer_sf(
    stat = StatSf,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    #show.legend = if (is.character(show.legend)) TRUE else show.legend,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      #legend = if (is.character(show.legend)) show.legend else "polygon",
      ...
    )
  )
}

