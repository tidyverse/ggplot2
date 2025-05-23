#' @include make-constructor.R
NULL

#' @rdname Geom
#' @format NULL
#' @usage NULL
#' @export
GeomPolygon <- ggproto("GeomPolygon", Geom,
  draw_panel = function(self, data, panel_params, coord, rule = "evenodd",
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    data <- fix_linewidth(data, snake_class(self))
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params, is_closed = TRUE)

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
          gp = gg_par(
            col = first_rows$colour,
            fill = fill_alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$linewidth,
            lty = first_rows$linetype,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
    } else {
      if (getRversion() < "3.6") {
        cli::cli_abort("Polygons with holes requires R 3.6 or above.")
      }
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique0(munched$subgroup))

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
          gp = gg_par(
            col = first_rows$colour,
            fill = fill_alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$linewidth,
            lty = first_rows$linetype,
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre
          )
        )
      )
    }
  },

  default_aes = aes(
    colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
    linewidth = from_theme(borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA, subgroup = NULL
  ),

  handle_na = function(data, params) {
    data
  },

  required_aes = c("x", "y"),

  draw_key = draw_key_polygon,

  rename_size = TRUE
)

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
#' @aesthetics GeomPolygon
#' @seealso
#'  [geom_path()] for an unfilled polygon,
#'  [geom_ribbon()] for a polygon anchored on the x-axis
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
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
#' set.seed(1)
#' stream <- data.frame(
#'   x = cumsum(runif(50, max = 0.1)),
#'   y = cumsum(runif(50,max = 0.1))
#' )
#'
#' p + geom_line(data = stream, colour = "grey30", linewidth = 5)
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
geom_polygon <- make_constructor(GeomPolygon)

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
