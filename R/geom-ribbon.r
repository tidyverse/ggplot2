#' Ribbons and area plots
#'
#' For each x value, `geom_ribbon()` displays a y interval defined
#' by `ymin` and `ymax`. `geom_area()` is a special case of
#' `geom_ribbon`, where the `ymin` is fixed to 0 and `y` is used instead
#' of `ymax`.
#'
#' An area plot is the continuous analogue of a stacked bar chart (see
#' [geom_bar()]), and can be used to show how composition of the
#' whole varies over the range of x. Choosing the order in which different
#' components is stacked is very important, as it becomes increasing hard to
#' see the individual pattern as you move up the stack. See
#' [position_stack()] for the details of stacking algorithm.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "ribbon")
#' @seealso
#'   [geom_bar()] for discrete intervals (bars),
#'   [geom_linerange()] for discrete intervals (lines),
#'   [geom_polygon()] for general polygons
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param outline.type Type of the outline of the area; `"both"` draws both the
#'   upper and lower lines, `"upper"`/`"lower"` draws the respective lines only.
#'   `"full"` draws a closed polygon around the area.
#' @export
#' @examples
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- ggplot(huron, aes(year))
#'
#' h + geom_ribbon(aes(ymin=0, ymax=level))
#' h + geom_area(aes(y = level))
#'
#' # Orientation cannot be deduced by mapping, so must be given explicitly for
#' # flipped orientation
#' h + geom_area(aes(x = level, y = year), orientation = "y")
#'
#' # Add aesthetic mappings
#' h +
#'   geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'   geom_line(aes(y = level))
geom_ribbon <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        outline.type = "both") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

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
      orientation = orientation,
      outline.type = outline.type,
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

  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)

    if (is.null(data$ymin) && is.null(data$ymax)) {
      abort(glue("Either ", flipped_names(params$flipped_aes)$ymin, " or ",
           flipped_names(params$flipped_aes)$ymax, " must be given as an aesthetic."))
    }
    data <- data[order(data$PANEL, data$group, data$x), , drop = FALSE]
    data$y <- data$ymin %||% data$ymax
    flip_data(data, params$flipped_aes)
  },

  draw_key = draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
    data <- flip_data(data, flipped_aes)
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      abort("Aesthetics can not vary with a ribbon")
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

    positions <- flip_data(positions, flipped_aes)

    munched <- coord_munch(coord, positions, panel_params)

    g_poly <- polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = gpar(
        fill = alpha(aes$fill, aes$alpha),
        col = if (identical(outline.type, "full")) aes$colour else NA
      )
    )

    if (identical(outline.type, "full")) {
      return(ggname("geom_ribbon", g_poly))
    }

    munched_lines <- munched
    # increment the IDs of the lower line
    munched_lines$id <- switch(outline.type,
      both = munched_lines$id + rep(c(0, max(ids, na.rm = TRUE)), each = length(ids)),
      upper = munched_lines$id + rep(c(0, NA), each = length(ids)),
      lower = munched_lines$id + rep(c(NA, 0), each = length(ids)),
      abort(glue("invalid outline.type: {outline.type}"))
    )
    g_lines <- polylineGrob(
      munched_lines$x, munched_lines$y, id = munched_lines$id,
      default.units = "native",
      gp = gpar(
        col = aes$colour,
        lwd = aes$size * .pt,
        lty = aes$linetype)
    )

    ggname("geom_ribbon", grobTree(g_poly, g_lines))
  }

)

#' @rdname geom_ribbon
#' @export
geom_area <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "stack", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...,
                      outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

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
      orientation = orientation,
      outline.type = outline.type,
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

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
    flip_data(data, params$flipped_aes)
  }
)
