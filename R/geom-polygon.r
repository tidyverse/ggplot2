#' Polygons
#'
#' Polygons are very similar to paths (as drawn by [geom_path()])
#' except that the start and end points are connected and the inside is
#' coloured by `fill`. The `group` aesthetic determines which cases
#' are connected together into a polygon. From R 3.6 and onwards it is possible
#' to draw polygons with holes by providing a subgroup aesthetic that
#' differentiates the outer ring points from those describing holes in the
#' polygon.
#'
#' @eval rd_aesthetics("geom", "polygon")
#' @seealso
#'  [geom_path()] for an unfilled polygon,
#'  [geom_ribbon()] for a polygon anchored on the x-axis
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param rule Either `"evenodd"` or `"winding"`. If polygons with holes are
#' being drawn (using the `subgroup` aesthetic) this argument defines how the
#' hole coordinates are interpreted. See the examples in [grid::pathGrob()] for
#' an explanation.
#' @examples
#' # When using geom_polygon, you will typically need two data frames:
#' # one contains the coordinates of each polygon (positions),  and the
#' # other the values associated with each polygon (values).  An id
#' # variable links the two together
#'
#' ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
#'
#' values <- data.frame(
#'   id = ids,
#'   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
#' )
#'
#' positions <- data.frame(
#'   id = rep(ids, each = 4),
#'   x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
#'   0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
#'   y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
#'   2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
#' )
#'
#' # Currently we need to manually merge the two together
#' datapoly <- merge(values, positions, by = c("id"))
#'
#' p <- ggplot(datapoly, aes(x = x, y = y)) +
#'   geom_polygon(aes(fill = value, group = id))
#' p
#'
#' # Which seems like a lot of work, but then it's easy to add on
#' # other features in this coordinate system, e.g.:
#'
#' stream <- data.frame(
#'   x = cumsum(runif(50, max = 0.1)),
#'   y = cumsum(runif(50,max = 0.1))
#' )
#'
#' p + geom_line(data = stream, colour = "grey30", size = 5)
#'
#' # And if the positions are in longitude and latitude, you can use
#' # coord_map to produce different map projections.
#'
#' if (packageVersion("grid") >= "3.6") {
#'   # As of R version 3.6 geom_polygon() supports polygons with holes
#'   # Use the subgroup aesthetic to differentiate holes from the main polygon
#'
#'   holes <- do.call(rbind, lapply(split(datapoly, datapoly$id), function(df) {
#'     df$x <- df$x + 0.5 * (mean(df$x) - df$x)
#'     df$y <- df$y + 0.5 * (mean(df$y) - df$y)
#'     df
#'   }))
#'   datapoly$subid <- 1L
#'   holes$subid <- 2L
#'   datapoly <- rbind(datapoly, holes)
#'
#'   p <- ggplot(datapoly, aes(x = x, y = y)) +
#'     geom_polygon(aes(fill = value, group = id, subgroup = subid))
#'   p
#' }
#'
geom_polygon <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         rule = "evenodd",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rule = rule,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolygon <- ggproto("GeomPolygon", Geom,
  draw_panel = function(data, panel_params, coord, rule = "evenodd", lineend = "butt",
                        linejoin = "round", linemitre = 10) {
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)

    if (is.null(munched$subgroup)) {
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      ggname(
        "geom_polygon",
        polygonGrob(
          munched$x, munched$y, default.units = "native",
          id = munched$group,
          gp = gpar(
            col = first_rows$colour,
            fill = alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$size * .pt,
            lty = first_rows$linetype,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
    } else {
      if (utils::packageVersion('grid') < "3.6") {
        abort("Polygons with holes requires R 3.6 or above")
      }
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique(munched$subgroup))

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      ggname(
        "geom_polygon",
        pathGrob(
          munched$x, munched$y, default.units = "native",
          id = id, pathId = munched$group,
          rule = rule,
          gp = gpar(
            col = first_rows$colour,
            fill = alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$size * .pt,
            lty = first_rows$linetype,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
    }

  },

  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA, subgroup = NULL),

  handle_na = function(data, params) {
    data
  },

  required_aes = c("x", "y"),

  draw_key = draw_key_polygon
)

# Assigning pathGrob in .onLoad ensures that packages that subclass GeomPolygon
# do not install with error `possible error in 'pathGrob(munched$x, munched$y, ':
# unused argument (pathId = munched$group)` despite the fact that this is correct
# usage
pathGrob <- NULL
on_load(
  if (getRversion() < as.numeric_version("3.6")) {
    pathGrob <- function(..., pathId.lengths) {
      grid::pathGrob(...)
    }
  }
)
