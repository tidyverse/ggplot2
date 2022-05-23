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

    if (inherits(coord, "CoordSf")) {
      # if the coord derives from CoordSf, then it
      # needs to know about bounding boxes of geometry data
      coord$record_bbox(
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
      )

      # to represent the location of the geometry in default coordinates,
      # we take the mid-point along each side of the bounding box and
      # backtransform
      bbox_trans <- sf_transform_xy(
        list(
          x = c(rep(0.5*(bbox[["xmin"]] + bbox[["xmax"]]), 2), bbox[["xmin"]], bbox[["xmax"]]),
          y = c(bbox[["ymin"]], bbox[["ymax"]], rep(0.5*(bbox[["ymin"]] + bbox[["ymax"]]), 2))
        ),
        coord$get_default_crs(),
        geometry_crs
      )

      # record as xmin, xmax, ymin, ymax so regular scales
      # have some indication of where shapes lie
      data$xmin <- min(bbox_trans$x)
      data$xmax <- max(bbox_trans$x)
      data$ymin <- min(bbox_trans$y)
      data$ymax <- max(bbox_trans$y)
    } else {
      # for all other coords, we record the full extent of the
      # geometry object
      data$xmin <- bbox[["xmin"]]
      data$xmax <- bbox[["xmax"]]
      data$ymin <- bbox[["ymin"]]
      data$ymax <- bbox[["ymax"]]
    }

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
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

