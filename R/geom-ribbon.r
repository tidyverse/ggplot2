#' Ribbons and area plots
#'
#' For each x value, `geom_ribbon` displays a y interval defined
#' by `ymin` and `ymax`. `geom_area` is a special case of
#' `geom_ribbon`, where the `ymin` is fixed to 0.
#'
#' An area plot is the continuous analogue of a stacked bar chart (see
#' [geom_bar()]), and can be used to show how composition of the
#' whole varies over the range of x. Choosing the order in which different
#' components is stacked is very important, as it becomes increasing hard to
#' see the individual pattern as you move up the stack. See
#' [position_stack()] for the details of stacking algorithm.
#'
#' @eval rd_aesthetics("geom", "ribbon")
#' @seealso
#'   [geom_bar()] for discrete intervals (bars),
#'   [geom_linerange()] for discrete intervals (lines),
#'   [geom_polygon()] for general polygons
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- ggplot(huron, aes(year))
#'
#' h + geom_ribbon(aes(ymin=0, ymax=level))
#' h + geom_area(aes(y = level))
#'
#' # Add aesthetic mappings
#' h +
#'   geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'   geom_line(aes(y = level))
geom_ribbon <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRibbon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRibbon <- ggproto("GeomRibbon", Geom,
  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "ymin", "ymax"),

  setup_data = function(data, params) {
    transform(data[order(data$PANEL, data$group, data$x), ], y = ymin)
  },

  draw_key = draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary with a ribbon")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    data <- unclass(data) #for faster indexing
    positions <- new_data_frame(list(
      x = c(data$x, rev(data$x)),
      y = c(data$ymax, rev(data$ymin)),
      id = c(ids, rev(ids))
    ))
    munched <- coord_munch(coord, positions, panel_params)

    ggname("geom_ribbon", polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = gpar(
        fill = alpha(aes$fill, aes$alpha),
        col = aes$colour,
        lwd = aes$size * .pt,
        lty = aes$linetype)
    ))
  }
)

#' @rdname geom_ribbon
#' @export
geom_area <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "stack", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArea,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomArea <- ggproto("GeomArea", GeomRibbon,
  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
  }
)
