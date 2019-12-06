#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
StatSf <- ggproto("StatSf", Stat,
  compute_layer = function(self, data, params, layout) {
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord <- layout$coord
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord) {
    geometry_data <- data[[ geom_column(data) ]]
    geometry_crs <- sf::st_crs(geometry_data)

    bbox <- sf::st_bbox(geometry_data)
    coord$record_bbox(
      xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
      ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
    )

    # register geometric center of each bbox, to give regular scales
    # some indication of where shapes lie
    bbox_trans <- sf_transform_xy(
      list(
        x = 0.5*(bbox[["xmin"]] + bbox[["xmax"]]),
        y = 0.5*(bbox[["ymin"]] + bbox[["ymax"]])
      ),
      coord$default_crs,
      geometry_crs
    )

    data$xmin <- bbox_trans$x
    data$xmax <- bbox_trans$x
    data$ymin <- bbox_trans$y
    data$ymax <- bbox_trans$y

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
    show.legend = if (is.character(show.legend)) TRUE else show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      legend = if (is.character(show.legend)) show.legend else "polygon",
      ...
    )
  )
}

