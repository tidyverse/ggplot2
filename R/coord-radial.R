
#' @rdname coord_polar
#'
#' @param end Position from 12 o'clock in radians where plot ends, to allow
#'   for partial polar coordinates. The default, `NULL`, is set to
#'   `start + 2 * pi`.
#' @param expand If `TRUE`, the default, adds a small expansion factor the
#'   the limits to prevent overlap between data and axes. If `FALSE`, limits
#'   are taken directly from the scale.
#' @param r.axis.inside If `TRUE`, places the radius axis inside the
#'   panel. If `FALSE`, places the radius axis next to the panel. The default,
#'   `NULL`, places the radius axis outside if the `start` and `end` arguments
#'   form a full circle.
#' @param rotate.angle If `TRUE`, transforms the `angle` aesthetic in data
#'   in accordance with the computed `theta` position. If `FALSE` (default),
#'   no such transformation is performed. Can be useful to rotate text geoms in
#'   alignment with the coordinates.
#' @param inner.radius A `numeric` between 0 and 1 setting the size of a inner.radius hole.
#' @param r_axis_inside,rotate_angle `r lifecycle::badge("deprecated")`
#'
#' @note
#' In `coord_radial()`, position guides are can be defined by using
#' `guides(r = ..., theta = ..., r.sec = ..., theta.sec = ...)`. Note that
#' these guides require `r` and `theta` as available aesthetics. The classic
#' `guide_axis()` can be used for the `r` positions and `guide_axis_theta()` can
#' be used for the `theta` positions. Using the `theta.sec` position is only
#' sensible when `inner.radius > 0`.
#'
#' @export
#' @examples
#' # A partial polar plot
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   coord_radial(start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3)
coord_radial <- function(theta = "x",
                         start = 0, end = NULL,
                         expand = TRUE,
                         direction = 1,
                         clip = "off",
                         r.axis.inside = NULL,
                         rotate.angle = FALSE,
                         inner.radius = 0,
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

  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  check_bool(r.axis.inside, allow_null = TRUE)
  check_bool(expand)
  check_bool(rotate.angle)
  check_number_decimal(start, allow_infinite = FALSE)
  check_number_decimal(end, allow_infinite = FALSE, allow_null = TRUE)
  check_number_decimal(inner.radius, min = 0, max = 1, allow_infinite = FALSE)

  end <- end %||% (start + 2 * pi)
  if (start > end) {
    n_rotate <- ((start - end) %/% (2 * pi)) + 1
    start <- start - n_rotate * 2 * pi
  }
  r.axis.inside <- r.axis.inside %||% !(abs(end - start) >= 1.999 * pi)

  ggproto(NULL, CoordRadial,
    theta = theta,
    r = r,
    arc = c(start, end),
    expand = expand,
    direction = sign(direction),
    r_axis_inside = r.axis.inside,
    rotate_angle = rotate.angle,
    inner_radius = c(inner.radius, 1) * 0.4,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordRadial <- ggproto("CoordRadial", Coord,

  aspect = function(details) {
    diff(details$bbox$y) / diff(details$bbox$x)
  },

  is_free = function() TRUE,

  distance = function(self, x, y, details) {
    arc <- details$arc %||% c(0, 2 * pi)
    if (self$theta == "x") {
      r <- rescale(y, from = details$r.range, to = self$inner_radius / 0.4)
      theta <- theta_rescale_no_clip(
        x, details$theta.range,
        arc, self$direction
      )
    } else {
      r <- rescale(x, from = details$r.range, to = self$inner_radius / 0.4)
      theta <- theta_rescale_no_clip(
        y, details$theta.range,
        arc, self$direction
      )
    }

    dist_polar(r, theta)
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
    c(
      view_scales_polar(scale_x, self$theta, expand = self$expand),
      view_scales_polar(scale_y, self$theta, expand = self$expand),
      list(bbox = polar_bbox(self$arc, inner_radius = self$inner_radius),
           arc = self$arc, inner_radius = self$inner_radius)
    )
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
      drop_guides <- check_polar_guide(drop_guides, guides, type)
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

    if (self$r_axis_inside) {

      arc <- rad2deg(self$arc)
      r_position <- c("left", "right")
      # If both opposite direction and opposite position, don't flip
      if (xor(self$direction == -1, opposite_r)) {
        arc <- rev(arc)
        r_position <- rev(r_position)
      }

      guide_params[["r"]]$position     <- r_position[1]
      guide_params[["r.sec"]]$position <- r_position[2]
      # Set guide text angles
      guide_params[["r"]]$angle     <- guide_params[["r"]]$angle     %|W|% arc[1]
      guide_params[["r.sec"]]$angle <- guide_params[["r.sec"]]$angle %|W|% arc[2]
    } else {
      r_position <- c(params$r_axis, opposite_position(params$r_axis))
      if (opposite_r) {
        r_position <- rev(r_position)
      }
      guide_params[["r"]]$position     <- r_position[1]
      guide_params[["r.sec"]]$position <- r_position[2]
    }

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
    for (t in intersect(c("theta", "theta.sec"), aesthetics[!empty])) {
      gdefs[[t]] <- guides[[t]]$train(gdefs[[t]], panel_params[[t]])
      gdefs[[t]] <- guides[[t]]$transform(gdefs[[t]], self, panel_params)
      gdefs[[t]] <- guides[[t]]$get_layer_key(gdefs[[t]], layers)
    }

    if (self$r_axis_inside) {
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
    for (r in intersect(c("r", "r.sec"), aesthetics[!empty])) {
      gdefs[[r]] <- guides[[r]]$train(gdefs[[r]], panel_params[[r]])
      gdefs[[r]] <- guides[[r]]$transform(gdefs[[r]], self, temp) # Use temp
      gdefs[[r]] <- guides[[r]]$get_layer_key(gdefs[[r]], layers)
    }

    # Set theme suffixes
    gdefs$theta$theme_suffix     <- "theta"
    gdefs$theta.sec$theme_suffix <- "theta"
    gdefs$r$theme_suffix         <- "r"
    gdefs$r.sec$theme_suffix     <- "r"

    panel_params$guides$update_params(gdefs)
    panel_params
  },

  transform = function(self, data, panel_params) {
    data <- rename_data(self, data)
    bbox <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    arc  <- panel_params$arc  %||% c(0, 2 * pi)

    data$r  <- r_rescale(data$r, panel_params$r.range, panel_params$inner_radius)
    data$theta <- theta_rescale(
      data$theta, panel_params$theta.range,
      arc, self$direction
    )
    data$x <- rescale(data$r * sin(data$theta) + 0.5, from = bbox$x)
    data$y <- rescale(data$r * cos(data$theta) + 0.5, from = bbox$y)

    if (self$rotate_angle && "angle" %in% names(data)) {
      data <- flip_data_text_angle(data)
    }

    data
  },

  render_axis_v = function(self, panel_params, theme) {
    if (self$r_axis_inside) {
      return(list(left = zeroGrob(), right = zeroGrob()))
    }
    CoordCartesian$render_axis_v(panel_params, theme)
  },

  render_axis_h = function(self, panel_params, theme) {
    if (self$r_axis_inside) {
      return(list(top = zeroGrob(), bottom = zeroGrob()))
    }
    CoordCartesian$render_axis_h(panel_params, theme)
  },

  render_bg = function(self, panel_params, theme) {

    bbox  <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    arc   <- panel_params$arc  %||% c(0, 2 * pi)
    dir   <- self$direction
    inner_radius <- panel_params$inner_radius

    theta_lim <- panel_params$theta.range
    theta_maj <- panel_params$theta.major
    theta_min <- setdiff(panel_params$theta.minor, theta_maj)

    if (length(theta_maj) > 0) {
      theta_maj <- theta_rescale(theta_maj, theta_lim, arc, dir)
    }
    if (length(theta_min) > 0) {
      theta_min <- theta_rescale(theta_min, theta_lim, arc, dir)
    }
    theta_fine <- seq(self$arc[1], self$arc[2], length.out = 100)

    r_fine <- r_rescale(panel_params$r.major, panel_params$r.range,
                         panel_params$inner_radius)

    # This gets the proper theme element for theta and r grid lines:
    #   panel.grid.major.x or .y
    grid_elems <- paste(
      c("panel.grid.major.", "panel.grid.minor.", "panel.grid.major."),
      c(self$theta, self$theta, self$r), sep = ""
    )
    grid_elems <- lapply(grid_elems, calc_element, theme = theme)
    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    bg_element <- calc_element("panel.background", theme)
    if (!inherits(bg_element, "element_blank")) {
      background <- data_frame0(
        x = c(Inf,  Inf, -Inf, -Inf),
        y = c(Inf, -Inf, -Inf,  Inf)
      )
      background <- coord_munch(self, background, panel_params, is_closed = TRUE)
      bg_gp <- ggpar(
        lwd = bg_element$linewidth,
        col = bg_element$colour, fill = bg_element$fill,
        lty = bg_element$linetype
      )
      background <- polygonGrob(
        x = background$x, y = background$y,
        gp = bg_gp
      )
    } else {
      background <- zeroGrob()
    }

    ggname("grill", grobTree(
      background,
      theta_grid(theta_maj, grid_elems[[1]], inner_radius, bbox),
      theta_grid(theta_min, grid_elems[[2]], inner_radius, bbox),
      element_render(
        theme, majorr, name = "radius",
        x = rescale(rep(r_fine, each = length(theta_fine)) *
          rep(sin(theta_fine), length(r_fine)) + 0.5, from = bbox$x),
        y = rescale(rep(r_fine, each = length(theta_fine)) *
          rep(cos(theta_fine), length(r_fine)) + 0.5, from = bbox$y),
        id.lengths = rep(length(theta_fine), length(r_fine)),
        default.units = "native"
      )
    ))
  },

  render_fg = function(self, panel_params, theme) {

    border <- element_render(theme, "panel.border", fill = NA)

    if (!self$r_axis_inside) {
      out <- grobTree(
        panel_guides_grob(panel_params$guides, "theta", theme),
        panel_guides_grob(panel_params$guides, "theta.sec", theme),
        border
      )
      return(out)
    }

    bbox <- panel_params$bbox
    dir  <- self$direction
    arc  <- if (dir == 1) self$arc else rev(self$arc)
    arc  <- dir * rad2deg(-arc)

    left <- panel_guides_grob(panel_params$guides, position = "left", theme)
    left <- rotate_r_axis(left, arc[1], bbox, "left")

    right <- panel_guides_grob(panel_params$guides, position = "right", theme)
    right <- rotate_r_axis(right, arc[2], bbox, "right")

    grobTree(
      panel_guides_grob(panel_params$guides, "theta", theme),
      panel_guides_grob(panel_params$guides, "theta.sec", theme),
      left, right,
      border
    )
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
    if (!self$r_axis_inside) {
      place <- in_arc(c(0, 0.5, 1, 1.5) * pi, self$arc)
      if (place[1]) {
        return(list(r_axis = "left", fake_arc = c(0, 2) * pi))
      }
      if (place[3]) {
        return(list(r_axis = "left", fake_arc = c(1, 3)* pi))
      }
      if (place[2]) {
        return(list(r_axis = "bottom", fake_arc = c(0.5, 2.5) * pi))
      }
      if (place[4]) {
        return(list(r_axis = "bottom", fake_arc = c(1.5, 3.5) * pi))
      }
      cli::cli_warn(c(
        "No appropriate placement found for {.arg r_axis_inside}.",
        i = "Axis will be placed at panel edge."
      ))
      self$r_axis_inside <- TRUE
    }
    return(NULL)
  }
)

view_scales_polar <- function(scale, theta = "x", expand = TRUE) {

  aesthetic <- scale$aesthetics[1]
  is_theta  <- theta == aesthetic
  name <- if (is_theta) "theta" else "r"

  expansion <- default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- expand_limits_scale(scale, expansion, limits)

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

  if (inherits(axis, "zeroGrob")) {
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

check_polar_guide <- function(drop_list, guides, type = "theta") {
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
