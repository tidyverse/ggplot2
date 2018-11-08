#' Map projections
#'
#' `coord_map` projects a portion of the earth, which is approximately
#' spherical, onto a flat 2D plane using any projection defined by the
#' `mapproj` package. Map projections do not, in general, preserve straight
#' lines, so this requires considerable computation. `coord_quickmap` is a
#' quick approximation that does preserve straight lines. It works best for
#' smaller areas closer to the equator.
#'
#' In general, map projections must account for the fact that the actual length
#' (in km) of one degree of longitude varies between the equator and the pole.
#' Near the equator, the ratio between the lengths of one degree of latitude and
#' one degree of longitude is approximately 1. Near the pole, it tends
#' towards infinity because the length of one degree of longitude tends towards
#' 0. For regions that span only a few degrees and are not too close to the
#' poles, setting the aspect ratio of the plot to the appropriate lat/lon ratio
#' approximates the usual mercator projection. This is what
#' `coord_quickmap` does, and is much faster (particularly for complex
#' plots like [geom_tile()]) at the expense of correctness.
#'
#' @param projection projection to use, see
#'    [mapproj::mapproject()] for list
#' @param ...,parameters Other arguments passed on to
#'   [mapproj::mapproject()]. Use `...` for named parameters to
#'   the projection, and `parameters` for unnamed parameters.
#'   `...` is ignored if the `parameters` argument is present.
#' @param orientation projection orientation, which defaults to
#'   `c(90, 0, mean(range(x)))`.  This is not optimal for many
#'   projections, so you will have to supply your own. See
#'   [mapproj::mapproject()] for more information.
#' @param xlim,ylim Manually specific x/y limits (in degrees of
#'   longitude/latitude)
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. For details, please see [`coord_cartesian()`].
#' @export
#' @examples
#' if (require("maps")) {
#' nz <- map_data("nz")
#' # Prepare a map of NZ
#' nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
#'   geom_polygon(fill = "white", colour = "black")
#'
#' # Plot it in cartesian coordinates
#' nzmap
#' # With correct mercator projection
#' nzmap + coord_map()
#' # With the aspect ratio approximation
#' nzmap + coord_quickmap()
#'
#' # Other projections
#' nzmap + coord_map("cylindrical")
#' nzmap + coord_map("azequalarea", orientation = c(-36.92, 174.6, 0))
#' nzmap + coord_map("lambert", parameters = c(-37, -44))
#'
#' states <- map_data("state")
#' usamap <- ggplot(states, aes(long, lat, group = group)) +
#'   geom_polygon(fill = "white", colour = "black")
#'
#' # Use cartesian coordinates
#' usamap
#' # With mercator projection
#' usamap + coord_map()
#' usamap + coord_quickmap()
#' # See ?mapproject for coordinate systems and their parameters
#' usamap + coord_map("gilbert")
#' usamap + coord_map("lagrange")
#'
#' # For most projections, you'll need to set the orientation yourself
#' # as the automatic selection done by mapproject is not available to
#' # ggplot
#' usamap + coord_map("orthographic")
#' usamap + coord_map("stereographic")
#' usamap + coord_map("conic", lat0 = 30)
#' usamap + coord_map("bonne", lat0 = 50)
#'
#' # World map, using geom_path instead of geom_polygon
#' world <- map_data("world")
#' worldmap <- ggplot(world, aes(x = long, y = lat, group = group)) +
#'   geom_path() +
#'   scale_y_continuous(breaks = (-2:2) * 30) +
#'   scale_x_continuous(breaks = (-4:4) * 45)
#'
#' # Orthographic projection with default orientation (looking down at North pole)
#' worldmap + coord_map("ortho")
#' # Looking up up at South Pole
#' worldmap + coord_map("ortho", orientation = c(-90, 0, 0))
#' # Centered on New York (currently has issues with closing polygons)
#' worldmap + coord_map("ortho", orientation = c(41, -74, 0))
#' }
coord_map <- function(projection="mercator", ..., parameters = NULL, orientation = NULL, xlim = NULL, ylim = NULL, clip = "on") {
  if (is.null(parameters)) {
    params <- list(...)
  } else {
    params <- parameters
  }

  ggproto(NULL, CoordMap,
    projection = projection,
    orientation = orientation,
    limits = list(x = xlim, y = ylim),
    params = params,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordMap <- ggproto("CoordMap", Coord,

  transform = function(self, data, panel_params) {
    trans <- mproject(self, data$x, data$y, panel_params$orientation)
    out <- cunion(trans[c("x", "y")], data)

    out$x <- rescale(out$x, 0:1, panel_params$x.proj)
    out$y <- rescale(out$y, 0:1, panel_params$y.proj)
    out
  },

  backtransform_range = function(panel_params) {
    # range is stored in data coordinates and doesn't have to be back-transformed
    list(x = panel_params$x.range, y = panel_params$y.range)
  },

  range = function(panel_params) {
    # Range in projected coordinates:
    #   list(x = panel_params$x.proj, y = panel_params$y.proj)
    # However, coord_map() does never really work with transformed coordinates,
    # so return unprojected data coordinates here
    list(x = panel_params$x.range, y = panel_params$y.range)
  },

  distance = function(x, y, panel_params) {
    max_dist <- dist_central_angle(panel_params$x.range, panel_params$y.range)
    dist_central_angle(x, y) / max_dist
  },

  aspect = function(ranges) {
    diff(ranges$y.proj) / diff(ranges$x.proj)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {

    # range in scale
    ranges <- list()
    for (n in c("x", "y")) {
      scale <- get(paste0("scale_", n))
      limits <- self$limits[[n]]

      if (is.null(limits)) {
        range <- scale$dimension(expand_default(scale))
      } else {
        range <- range(scale$transform(limits))
      }
      ranges[[n]] <- range
    }

    orientation <- self$orientation %||% c(90, 0, mean(ranges$x))

    # Increase chances of creating valid boundary region
    grid <- expand.grid(
      x = seq(ranges$x[1], ranges$x[2], length.out = 50),
      y = seq(ranges$y[1], ranges$y[2], length.out = 50)
    )

    ret <- list(x = list(), y = list())

    # range in map
    proj <- mproject(self, grid$x, grid$y, orientation)$range
    ret$x$proj <- proj[1:2]
    ret$y$proj <- proj[3:4]

    for (n in c("x", "y")) {
      out <- get(paste0("scale_", n))$break_info(ranges[[n]])
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$labels <- out$labels
    }

    details <- list(
      orientation = orientation,
      x.range = ret$x$range, y.range = ret$y$range,
      x.proj = ret$x$proj, y.proj = ret$y$proj,
      x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
      y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels
    )
    details
  },

  render_bg = function(self, panel_params, theme) {
    xrange <- expand_range(panel_params$x.range, 0.2)
    yrange <- expand_range(panel_params$y.range, 0.2)

    # Limit ranges so that lines don't wrap around globe
    xmid <- mean(xrange)
    ymid <- mean(yrange)
    xrange[xrange < xmid - 180] <- xmid - 180
    xrange[xrange > xmid + 180] <- xmid + 180
    yrange[yrange < ymid - 90] <- ymid - 90
    yrange[yrange > ymid + 90] <- ymid + 90

    xgrid <- with(panel_params, expand.grid(
      y = c(seq(yrange[1], yrange[2], length.out = 50), NA),
      x = x.major
    ))
    ygrid <- with(panel_params, expand.grid(
      x = c(seq(xrange[1], xrange[2], length.out = 50), NA),
      y = y.major
    ))

    xlines <- self$transform(xgrid, panel_params)
    ylines <- self$transform(ygrid, panel_params)

    if (nrow(xlines) > 0) {
      grob.xlines <- element_render(
        theme, "panel.grid.major.x",
        xlines$x, xlines$y, default.units = "native"
      )
    } else {
      grob.xlines <- zeroGrob()
    }

    if (nrow(ylines) > 0) {
      grob.ylines <- element_render(
        theme, "panel.grid.major.y",
        ylines$x, ylines$y, default.units = "native"
      )
    } else {
      grob.ylines <- zeroGrob()
    }

    ggname("grill", grobTree(
      element_render(theme, "panel.background"),
      grob.xlines, grob.ylines
    ))
  },

  render_axis_h = function(self, panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("primary", "secondary")

    if (is.null(panel_params$x.major)) {
      return(list(
        top = zeroGrob(),
        bottom = zeroGrob()
      ))
    }

    x_intercept <- with(panel_params, new_data_frame(list(
      x = x.major,
      y = rep(y.range[1], length(x.major))
    )))
    pos <- self$transform(x_intercept, panel_params)

    axes <- list(
      bottom = guide_axis(pos$x, panel_params$x.labels, "bottom", theme),
      top = guide_axis(pos$x, panel_params$x.labels, "top", theme)
    )
    axes[[which(arrange == "secondary")]] <- zeroGrob()
    axes
  },

  render_axis_v = function(self, panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")

    if (is.null(panel_params$y.major)) {
      return(list(
        left = zeroGrob(),
        right = zeroGrob()
      ))
    }

    x_intercept <- with(panel_params, new_data_frame(list(
      x = rep(x.range[1], length(y.major)),
      y = y.major
    )))
    pos <- self$transform(x_intercept, panel_params)

    axes <- list(
      left = guide_axis(pos$y, panel_params$y.labels, "left", theme),
      right = guide_axis(pos$y, panel_params$y.labels, "right", theme)
    )
    axes[[which(arrange == "secondary")]] <- zeroGrob()
    axes
  }
)


mproject <- function(coord, x, y, orientation) {
  suppressWarnings(mapproj::mapproject(x, y,
    projection = coord$projection,
    parameters  = coord$params,
    orientation = orientation
  ))
}
