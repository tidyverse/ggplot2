#' Polar coordinates
#'
#' The polar coordinate system is most commonly used for pie charts, which
#' are a stacked bar chart in polar coordinates. \cr
#' `r lifecycle::badge("superseded")`: `coord_polar()` has been in favour of
#' `coord_radial()`.
#'
#' @param theta variable to map angle to (`x` or `y`)
#' @param start Offset of starting point from 12 o'clock in radians. Offset
#'   is applied clockwise or anticlockwise depending on value of `direction`.
#' @param direction 1, clockwise; -1, anticlockwise
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. For details, please see [`coord_cartesian()`].
#' @param end Position from 12 o'clock in radians where plot ends, to allow
#'   for partial polar coordinates. The default, `NULL`, is set to
#'   `start + 2 * pi`.
#' @param thetalim,rlim Limits for the theta and r axes.
#' @param expand If `TRUE`, the default, adds a small expansion factor to
#'   the limits to prevent overlap between data and axes. If `FALSE`, limits
#'   are taken directly from the scale.
#' @param r.axis.inside One of the following:
#'   * `NULL` (default) places the axis next to the panel if `start` and
#'     `end` arguments form a full circle and inside the panel otherwise.
#'   * `TRUE` to place the radius axis inside the panel.
#'   * `FALSE` to place the radius axis next to the panel.
#'   * A numeric value, setting a theta axis value at which
#'     the axis should be placed inside the panel. Can be given as a length 2
#'     vector to control primary and secondary axis placement separately.
#' @param rotate.angle If `TRUE`, transforms the `angle` aesthetic in data
#'   in accordance with the computed `theta` position. If `FALSE` (default),
#'   no such transformation is performed. Can be useful to rotate text geoms in
#'   alignment with the coordinates.
#' @param inner.radius A `numeric` between 0 and 1 setting the size of a
#'   inner radius hole.
#' @param reverse A string giving which directions to reverse. `"none"`
#'   (default) keep directions as is. `"theta"` reverses the angle and `"r"`
#'   reverses the radius. `"thetar"` reverses both the angle and the radius.
#' @param r_axis_inside,rotate_angle `r lifecycle::badge("deprecated")`
#' @export
#' @note
#' In `coord_radial()`, position guides can be defined by using
#' `guides(r = ..., theta = ..., r.sec = ..., theta.sec = ...)`. Note that
#' these guides require `r` and `theta` as available aesthetics. The classic
#' [guide_axis()] can be used for the `r` positions and [guide_axis_theta()] can
#' be used for the `theta` positions. Using the `theta.sec` position is only
#' sensible when `inner.radius > 0`.
#'
#' @seealso
#' The `r link_book("polar coordinates section", "coord#polar-coordinates-with-coord_polar")`
#' @examples
#' # NOTE: Use these plots with caution - polar coordinates has
#' # major perceptual problems.  The main point of these examples is
#' # to demonstrate how these common plots can be described in the
#' # grammar.  Use with EXTREME caution.
#'
#' # A pie chart = stacked bar chart + polar coordinates
#' pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'  geom_bar(width = 1)
#' pie + coord_radial(theta = "y", expand = FALSE)
#'
#' \donttest{
#'
#' # A coxcomb plot = bar chart + polar coordinates
#' cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
#'   geom_bar(width = 1, colour = "black")
#' cxc + coord_radial(expand = FALSE)
#' # A new type of plot?
#' cxc + coord_radial(theta = "y", expand = FALSE)
#'
#' # The bullseye chart
#' pie + coord_radial(expand = FALSE)
#'
#' # Hadley's favourite pie chart
#' df <- data.frame(
#'   variable = c("does not resemble", "resembles"),
#'   value = c(20, 80)
#' )
#' ggplot(df, aes(x = "", y = value, fill = variable)) +
#'   geom_col(width = 1) +
#'   scale_fill_manual(values = c("red", "yellow")) +
#'   coord_radial("y", start = pi / 3, expand =  FALSE) +
#'   labs(title = "Pac man")
#'
#' # Windrose + doughnut plot
#' if (require("ggplot2movies")) {
#' movies$rrating <- cut_interval(movies$rating, length = 1)
#' movies$budgetq <- cut_number(movies$budget, 4)
#'
#' doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
#'
#' # Wind rose
#' doh + geom_bar(width = 1) + coord_radial(expand = FALSE)
#' # Race track plot
#' doh + geom_bar(width = 0.9, position = "fill") +
#'   coord_radial(theta = "y", expand = FALSE)
#' }
#' }
#' # A partial polar plot
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   coord_radial(start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3)
#'
#' # Similar with coord_cartesian(), you can set limits.
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   coord_radial(
#'     start = -0.4 * pi,
#'     end = 0.4 * pi, inner.radius = 0.3,
#'     thetalim = c(200, 300),
#'     rlim = c(15, 30),
#'   )
coord_radial <- function(theta = "x",
                         start = 0, end = NULL,
                         thetalim = NULL, rlim = NULL, expand = TRUE,
                         direction = deprecated(),
                         clip = "off",
                         r.axis.inside = NULL,
                         rotate.angle = FALSE,
                         inner.radius = 0,
                         reverse = "none",
                         r_axis_inside = deprecated(),
                         rotate_angle = deprecated()) {

  if (lifecycle::is_present(r_axis_inside)) {
    deprecate_warn0(
      "3.5.1", "coord_radial(r_axis_inside)", "coord_radial(r.axis.inside)"
    )
    r.axis.inside <- r_axis_inside
  }
  if (lifecycle::is_present(rotate_angle)) {
    deprecate_warn0(
      "3.5.1", "coord_radial(rotate_angle)", "coord_radial(rotate.angle)"
    )
    rotate.angle <- rotate_angle
  }
  if (lifecycle::is_present(direction)) {
    deprecate_warn0(
      "4.0.0", "coord_radial(direction)", "coord_radial(reverse)"
    )
    reverse <- switch(reverse, "r" = "thetar", "theta")
  }

  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  if (!is.numeric(r.axis.inside)) {
    check_bool(r.axis.inside, allow_null = TRUE)
  }
  reverse <- arg_match0(reverse, c("theta", "thetar", "r", "none"))

  check_bool(rotate.angle)
  check_number_decimal(start, allow_infinite = FALSE)
  check_number_decimal(end, allow_infinite = FALSE, allow_null = TRUE)
  check_number_decimal(inner.radius, min = 0, max = 1, allow_infinite = FALSE)

  arc <- c(start, end %||% (start + 2 * pi))
  if (arc[1] > arc[2]) {
    n_rotate <- ((arc[1] - arc[2]) %/% (2 * pi)) + 1
    arc[1] <- arc[1] - n_rotate * 2 * pi
  }
  arc <- switch(reverse, thetar = , theta = rev(arc), arc)

  r.axis.inside <- r.axis.inside %||% !(abs(arc[2] - arc[1]) >= 1.999 * pi)
  if (isFALSE(r.axis.inside)) {
    place <- in_arc(c(0, 0.5, 1, 1.5) * pi, arc)
    if (!any(place)) {
      cli::cli_warn(c(
        "No appropriate placement found for outside {.field r.axis}.",
        i = "Will use {.code r.axis.inside = TRUE} instead"
      ))
      r.axis.inside <- TRUE
    }
  }

  inner.radius <- c(inner.radius, 1) * 0.4
  inner.radius <- switch(reverse, thetar = , r = rev, identity)(inner.radius)

  ggproto(NULL, CoordRadial,
    limits = list(theta = thetalim, r = rlim),
    theta = theta,
    r = r,
    arc = arc,
    expand = expand,
    reverse = reverse,
    r_axis_inside = r.axis.inside,
    rotate_angle = rotate.angle,
    inner_radius = inner.radius,
    clip = clip
  )
}

#' @rdname Coord
#' @format NULL
#' @usage NULL
#' @export
CoordRadial <- ggproto("CoordRadial", Coord,

  aspect = function(details) {
    diff(details$bbox$y) / diff(details$bbox$x)
  },

  is_free = function() {
    TRUE
  },

  distance = function(self, x, y, details, boost = 0.75) {
    arc <- details$arc %||% c(0, 2 * pi)
    if (self$theta == "x") {
      r <- rescale(y, from = details$r.range, to = self$inner_radius / 0.4)
      theta <- theta_rescale_no_clip(x, details$theta.range, arc)
    } else {
      r <- rescale(x, from = details$r.range, to = self$inner_radius / 0.4)
      theta <- theta_rescale_no_clip(y, details$theta.range, arc)
    }
    # The ^boost boosts detailed munching when r is small
    dist_polar(r^boost, theta)
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects that the x and y ranges here
    # match the setting from self$theta and self$r
    setNames(
      list(panel_params$theta.range, panel_params$r.range),
      c(self$theta, self$r)
    )
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    if (self$theta == "x") {
      xlimits <- self$limits$theta
      ylimits <- self$limits$r
    } else {
      xlimits <- self$limits$r
      ylimits <- self$limits$theta
    }
    params <- c(
      view_scales_polar(scale_x, self$theta, xlimits,
        expand = params$expand[c(4, 2)]
      ),
      view_scales_polar(scale_y, self$theta, ylimits,
        expand = params$expand[c(3, 1)]
      ),
      list(bbox = polar_bbox(self$arc, inner_radius = self$inner_radius),
           arc = self$arc, inner_radius = self$inner_radius)
    )

    axis_rotation <- self$r_axis_inside
    if (is.numeric(axis_rotation)) {
      theta_scale <- switch(self$theta, x = scale_x, y = scale_y)
      axis_rotation <- theta_scale$transform(axis_rotation)
      axis_rotation <- oob_squish(axis_rotation, params$theta.range)
      axis_rotation <- theta_rescale(
        axis_rotation, params$theta.range,
        params$arc, 1
      )
      params$axis_rotation <- rep_len(axis_rotation, length.out = 2)
    } else {
      params$axis_rotation <- params$arc
    }

    params
  },

  setup_panel_guides = function(self, panel_params, guides, params = list()) {

    aesthetics <- c("r", "theta", "r.sec", "theta.sec")
    names(aesthetics) <- aesthetics
    is_sec <- grepl("sec$", aesthetics)
    scales <- panel_params[aesthetics]

    # Fill in theta guide default
    panel_params$theta$guide <- panel_params$theta$guide %|W|% guide_axis_theta()

    guides <- guides$setup(
      scales, aesthetics,
      default = params$guide_default %||% guide_axis(),
      missing = params$guide_missing %||% guide_none()
    )

    # Validate appropriateness of guides
    drop_guides <- character(0)
    for (type in aesthetics) {
      drop_guides <- validate_polar_guide(drop_guides, guides, type)
    }

    guide_params <- guides$get_params(aesthetics)
    names(guide_params) <- aesthetics

    # Set guide positions
    guide_params[["theta"]]$position     <- "theta"
    guide_params[["theta.sec"]]$position <- "theta.sec"

    if (self$theta == "x") {
      opposite_r <- isTRUE(scales$r$position %in% c("top", "right"))
    } else {
      opposite_r <- isTRUE(scales$r$position %in% c("bottom", "left"))
    }

    if (!isFALSE(self$r_axis_inside)) {

      r_position <- c("left", "right")
      # If both opposite direction and opposite position, don't flip
      if (xor(self$reverse %in% c("thetar", "theta"), opposite_r)) {
        r_position <- rev(r_position)
      }
      arc <- rad2deg(panel_params$axis_rotation)
      if (opposite_r) {
        arc <- rev(arc)
      }
      # Set guide text angles
      guide_params[["r"]]$angle     <- guide_params[["r"]]$angle     %|W|% arc[1]
      guide_params[["r.sec"]]$angle <- guide_params[["r.sec"]]$angle %|W|% arc[2]
    } else {
      r_position <- c(params$r_axis, opposite_position(params$r_axis))
      if (opposite_r) {
        r_position <- rev(r_position)
      }
    }
    guide_params[["r"]]$position     <- r_position[1]
    guide_params[["r.sec"]]$position <- r_position[2]

    guide_params[drop_guides] <- list(NULL)
    guides$update_params(guide_params)

    panel_params$guides <- guides
    panel_params
  },

  train_panel_guides = function(self, panel_params, layers, params = list()) {

    aesthetics <- c("r", "theta", "r.sec", "theta.sec")
    aesthetics <- intersect(aesthetics, names(panel_params$guides$aesthetics))
    names(aesthetics) <- aesthetics

    guides <- panel_params$guides$get_guide(aesthetics)
    names(guides) <- aesthetics
    empty  <- vapply(guides, inherits, logical(1), "GuideNone")
    gdefs <- panel_params$guides$get_params(aesthetics)
    names(gdefs) <- aesthetics

    # Train theta guide
    t <- intersect(c("theta", "theta.sec"), aesthetics[!empty])
    gdefs[t] <- Map(
      function(guide, guide_param, scale) {
        guide_param$theme_suffix <- "theta"
        guide_param <- guide$train(guide_param, scale)
        guide_param <- guide$transform(guide_param, self, panel_params)
        guide_param <- guide$get_layer_key(guide_param, layers)
      },
      guide = guides[t],
      guide_param = gdefs[t],
      scale = panel_params[t]
    )

    if (!isFALSE(self$r_axis_inside)) {
      # For radial axis, we need to pretend that rotation starts at 0 and
      # the bounding box is for circles, otherwise tick positions will be
      # spaced too closely.
      mod <- list(bbox = list(x = c(0, 1), y = c(0, 1)), arc = c(0, 2 * pi))
    } else {
      # When drawing radial axis outside, we need to pretend that arcs starts
      # at horizontal or vertical position to have the transform work right.
      mod <- list(arc = params$fake_arc)
    }
    temp <- modify_list(panel_params, mod)

    # Train radial guide
    r <- intersect(c("r", "r.sec"), aesthetics[!empty])
    gdefs[r] <- Map(
      function(guide, guide_param, scale) {
        guide_param$theme_suffix <- "r"
        guide_param <- guide$train(guide_param, scale)
        guide_param <- guide$transform(guide_param, self, temp)
        guide_param <- guide$get_layer_key(guide_param, layers)
      },
      guide = guides[r],
      guide_param = gdefs[r],
      scale = panel_params[r]
    )

    panel_params$guides$update_params(gdefs)
    panel_params
  },

  transform = function(self, data, panel_params) {
    if (is_transform_immune(data, snake_class(self))) {
      return(data)
    }

    data <- rename_data(self, data)
    bbox <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    arc  <- panel_params$arc  %||% c(0, 2 * pi)

    data$r  <- r_rescale(data$r, panel_params$r.range, panel_params$inner_radius)
    data$theta <- theta_rescale(data$theta, panel_params$theta.range, arc)
    data$x <- rescale(data$r * sin(data$theta) + 0.5, from = bbox$x)
    data$y <- rescale(data$r * cos(data$theta) + 0.5, from = bbox$y)

    if (self$rotate_angle && "angle" %in% names(data)) {
      data <- flip_data_text_angle(data)
    }

    data
  },

  render_axis_v = function(self, panel_params, theme) {
    if (!isFALSE(self$r_axis_inside)) {
      return(list(left = zeroGrob(), right = zeroGrob()))
    }
    CoordCartesian$render_axis_v(panel_params, theme)
  },

  render_axis_h = function(self, panel_params, theme) {
    if (!isFALSE(self$r_axis_inside)) {
      return(list(top = zeroGrob(), bottom = zeroGrob()))
    }
    CoordCartesian$render_axis_h(panel_params, theme)
  },

  render_bg = function(self, panel_params, theme) {
    panel_params <- switch(
      self$theta,
      x = rename(panel_params, c(theta = "x", r = "y")),
      y = rename(panel_params, c(theta = "y", r = "x"))
    )
    guide_grid(theme, panel_params, self, square = FALSE)
  },

  render_fg = function(self, panel_params, theme) {

    border <- element_render(theme, "panel.border", fill = NA)

    if (isFALSE(self$r_axis_inside)) {
      out <- grobTree(
        panel_guides_grob(panel_params$guides, "theta", theme),
        panel_guides_grob(panel_params$guides, "theta.sec", theme),
        border
      )
      return(out)
    }

    bbox <- panel_params$bbox
    dir  <- self$direction
    rot  <- panel_params$axis_rotation
    rot  <- switch(self$reverse, thetar = , theta = rev(rot), rot)
    rot  <- rad2deg(-rot)

    left <- panel_guides_grob(panel_params$guides, position = "left", theme)
    left <- rotate_r_axis(left, rot[1], bbox, "left")

    right <- panel_guides_grob(panel_params$guides, position = "right", theme)
    right <- rotate_r_axis(right, rot[2], bbox, "right")

    grobTree(
      panel_guides_grob(panel_params$guides, "theta", theme),
      panel_guides_grob(panel_params$guides, "theta.sec", theme),
      left, right,
      border
    )
  },


  draw_panel = function(self, panel, params, theme) {
    clip_support <- check_device("clippingPaths", "test", maybe = TRUE)
    if (self$clip == "on" && !isFALSE(clip_support)) {
      clip_path <- data_frame0(
        x = c(Inf, Inf, -Inf, -Inf),
        y = c(Inf, -Inf, -Inf, Inf)
      )
      clip_path <- coord_munch(self, clip_path, params, is_closed = TRUE)
      clip_path <- polygonGrob(clip_path$x, clip_path$y)
      # Note that clipping path is applied to panel without coord
      # foreground/background (added in parent method).
      # These may contain decorations that needn't be clipped
      panel <- list(gTree(
        children = inject(gList(!!!panel)),
        vp = viewport(clip = clip_path)
      ))
    }
    ggproto_parent(Coord, self)$draw_panel(panel, params, theme)
  },

  labels = function(self, labels, panel_params) {
    # `Layout$resolve_label()` doesn't know to look for theta/r/r.sec guides,
    # so we'll handle title propagation here.
    titles <- lapply(
      panel_params$guides$get_params(c("theta", "r", "r.sec")),
      function(x) x$title
    )
    if (self$theta == "y") {
      # Need to use single brackets for labels to avoid deleting an element by
      # assigning NULL
      labels$y['primary']   <- list(titles[[1]] %|W|% labels$y$primary)
      labels$x['primary']   <- list(titles[[2]] %|W|% labels$x$primary)
      labels$x['secondary'] <- list(titles[[3]] %|W|% labels$x$secondary)
      if (any(in_arc(c(0, 1) * pi, panel_params$arc))) {
        labels <- list(x = labels$y, y = labels$x)
      } else {
        labels <- list(x = rev(labels$x), y = rev(labels$y))
      }
    } else {
      labels$x['primary']   <- list(titles[[1]] %|W|% labels$x$primary)
      labels$y['primary']   <- list(titles[[2]] %|W|% labels$y$primary)
      labels$y['secondary'] <- list(titles[[3]] %|W|% labels$y$secondary)

      if (!any(in_arc(c(0, 1) * pi, panel_params$arc))) {
        labels <- list(x = rev(labels$y), y = rev(labels$x))
      }
    }
    labels
  },

  modify_scales = function(self, scales_x, scales_y) {
    if (self$theta != "y")
      return()

    lapply(scales_x, scale_flip_position)
    lapply(scales_y, scale_flip_position)
  },

  setup_params = function(self, data) {
    params <- ggproto_parent(Coord, self)$setup_params(data)
    if (isFALSE(self$r_axis_inside)) {
      place <- in_arc(c(0, 0.5, 1, 1.5) * pi, self$arc)
      params$r_axis   <- if (any(place[c(1, 3)])) "left" else "bottom"
      params$fake_arc <- switch(
        which(place[c(1, 3, 2, 4)])[1],
        c(0, 2), c(1, 3), c(0.5, 2.5), c(1.5, 3.5)
      ) * pi
    }
    params
  }
)

view_scales_polar <- function(scale, theta = "x", coord_limits = NULL,
                              expand = TRUE) {

  aesthetic <- scale$aesthetics[1]
  is_theta  <- theta == aesthetic
  name <- if (is_theta) "theta" else "r"

  expansion <- default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- expand_limits_scale(
    scale, expansion, limits,
    coord_limits
  )

  primary <- view_scale_primary(scale, limits, continuous_range)
  view_scales <- list(
    primary,
    sec = view_scale_secondary(scale, limits, continuous_range),
    major = primary$map(primary$get_breaks()),
    minor = primary$map(primary$get_breaks_minor()),
    range = continuous_range
  )

  names(view_scales) <- c(name, paste0(name, ".", names(view_scales)[-1]))
  view_scales
}

#' Bounding box for partial polar coordinates
#'
#' Calculates the appropriate area to display a partial polar plot.
#'
#' @param arc The theta limits of the arc spanned by the partial polar plot.
#' @param margin A `numeric[4]` giving the margin that should be added to the
#'   top, right, bottom and left to the plot at edges that are shortened.
#'
#' @return A `list` with element `x`, containing the 'xmin' and 'xmax', and
#'   element `y` giving 'ymin' and 'ymax' of the bounding box.
#'
#' @noRd
#' @examples
#' polar_bbox(c(0, 1) * pi)
polar_bbox <- function(arc, margin = c(0.05, 0.05, 0.05, 0.05),
                       inner_radius = c(0, 0.4)) {

  # Early exit if we have full circle or more
  if (abs(diff(arc)) >= 2 * pi) {
    return(list(x = c(0, 1), y = c(0, 1)))
  }
  arc <- sort(arc)
  inner_radius <- sort(inner_radius)

  # X and Y position of the sector arc ends
  xmax <- 0.5 * sin(arc) + 0.5
  ymax <- 0.5 * cos(arc) + 0.5
  xmin <- inner_radius[1] * sin(arc) + 0.5
  ymin <- inner_radius[1] * cos(arc) + 0.5

  margin <- c(
    max(ymin) + margin[1],
    max(xmin) + margin[2],
    min(ymin) - margin[3],
    min(xmin) - margin[4]
  )

  # Check for top, right, bottom and left if it falls in sector
  pos_theta <- seq(0, 1.5 * pi, length.out = 4)
  in_sector <- in_arc(pos_theta, arc)

  bounds <- ifelse(
    in_sector,
    c(1, 1, 0, 0),
    c(max(ymax, margin[1]), max(xmax, margin[2]),
      min(ymax, margin[3]), min(xmax, margin[4]))
  )
  list(x = c(bounds[4], bounds[2]),
       y = c(bounds[3], bounds[1]))
}

# For any `theta` in [0, 2 * pi), test if theta is inside the span
# given by `arc`
in_arc <- function(theta, arc) {
  # Full circle case
  if (abs(diff(arc)) > 2 * pi - sqrt(.Machine$double.eps)) {
    return(rep(TRUE, length(theta)))
  }
  # Partial circle case
  arc <- arc %% (2 * pi)
  if (arc[1] < arc[2]) {
    theta >= arc[1] & theta <= arc[2]
  } else {
    !(theta < arc[1] & theta > arc[2])
  }
}

# Helpers to convert degrees to radians and vice versa
rad2deg <- function(rad) rad * 180 / pi
deg2rad <- function(deg) deg * pi / 180

# Function to rotate a radius axis through viewport
rotate_r_axis <- function(axis, angle, bbox, position = "left") {

  if (is_zero(axis)) {
    return(axis)
  }

  gTree(
    children = gList(axis),
    vp = viewport(
      angle = angle,
      x = unit(rescale(0.5, from = bbox$x), "npc"),
      y = unit(rescale(0.5, from = bbox$y), "npc"),
      just = c(as.numeric(position == "left"), 0.5),
      height = unit(1 / diff(bbox$y), "npc")
    )
  )
}

flip_text_angle <- function(angle) {
  angle <- angle %% 360
  flip <- angle > 90 & angle < 270
  angle[flip] <- angle[flip] + 180
  angle
}

flip_data_text_angle <- function(data) {
  if (!all(c("angle", "theta") %in% names(data))) {
    return(data)
  }
  angle <- (data$angle - rad2deg(data$theta)) %% 360
  flip  <- angle > 90 & angle < 270
  angle[flip] <- angle[flip] + 180
  data$angle <- angle
  if ("hjust" %in% names(data)) {
    data$hjust[flip] <- 1 - data$hjust[flip]
  }
  if ("vjust" %in% names(data)) {
    data$vjust[flip] <- 1 - data$vjust[flip]
  }
  data
}


theta_grid <- function(theta, element, inner_radius = c(0, 0.4),
                       bbox = list(x = c(0, 1), y = c(0, 1))) {
  n <- length(theta)
  if (n < 1) {
    return(NULL)
  }

  inner_radius <- rep(inner_radius, n)
  x <- rep(sin(theta), each = 2) * inner_radius + 0.5
  y <- rep(cos(theta), each = 2) * inner_radius + 0.5

  element_grob(
    element,
    x = rescale(x, from = bbox$x),
    y = rescale(y, from = bbox$y),
    id.lengths = rep(2, n),
    default.units = "native"
  )
}

validate_polar_guide <- function(drop_list, guides, type = "theta") {
  guide <- guides$get_guide(type)
  primary <- gsub("\\.sec$", "", type)
  if (inherits(guide, "GuideNone") || primary %in% guide$available_aes) {
    return(drop_list)
  }
  cli::cli_warn(c(
    "{.fn {snake_class(guide)}} cannot be used for {.field {primary}}.",
    i = "Use {?one of} {.or {.field {guide$available_aes}}} instead."
  ))
  c(drop_list, type)
}
