#' @include guide-axis.R
NULL

#' Angle axis guide
#'
#' This is a specialised guide used in `coord_radial()` to represent the theta
#' position scale.
#'
#' @inheritParams guide_axis
#'
#' @note
#' The axis labels in this guide are insensitive to `hjust` and `vjust`
#' settings. The distance from the tick marks to the labels is determined by
#' the largest `margin` size set in the theme.
#'
#' @export
#'
#' @examples
#' # A plot using coord_radial
#' p <- ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   coord_radial()
#'
#' # The `angle` argument can be used to set relative angles
#' p + guides(theta = guide_axis_theta(angle = 0))
guide_axis_theta <- function(title = waiver(), angle = waiver(),
                             minor.ticks = FALSE, cap = "none", order = 0,
                             position = waiver()) {

  check_bool(minor.ticks)
  if (is.logical(cap)) {
    check_bool(cap)
    cap <- if (cap) "both" else "none"
  }
  cap <- arg_match0(cap, c("none", "both", "upper", "lower"))

  new_guide(
    title = title,

    # customisations
    angle = angle,
    cap = cap,
    minor.ticks  = minor.ticks,

    # parameter
    available_aes = c("x", "y", "theta"),

    # general
    order = order,
    position = position,
    name = "axis",
    super = GuideAxisTheta
  )
}

GuideAxisTheta <- ggproto(
  "GuideAxisTheta", GuideAxis,

  extract_decor = function(scale, aesthetic, key, cap = "none", position, ...) {
    # For theta position, we pretend we're left/right because that will put
    # the correct opposite aesthetic as the line coordinates.
    position <- switch(position, theta = "left", theta.sec = "right", position)

    GuideAxis$extract_decor(
      scale = scale, aesthetic = aesthetic,
      position = position, key = key, cap = cap
    )
  },

  transform = function(params, coord, panel_params) {

    opposite <- setdiff(c("x", "y"), params$aesthetic)
    params$key[[opposite]] <- switch(params$position,
                                     theta.sec = -Inf,
                                     top = -Inf,
                                     right = -Inf,
                                     Inf)

    params <- GuideAxis$transform(params, coord, panel_params)

    key <- params$key
    n <- nrow(key)

    params$theme_aes <- coord$theta %||% params$aesthetic

    if (!("theta" %in% names(key))) {
      # We likely have a linear coord, so we match the text angles to
      # standard axes to be visually similar.
      key$theta <- switch(
        params$position,
        top    = 0,
        bottom = 1   * pi,
        left   = 1.5 * pi,
        right  = 0.5 * pi
      )
    } else {
      if (params$position == 'theta.sec') {
        key$theta <- key$theta + pi
      }

      # If the first and last positions are close together, we merge the
      # labels of these positions
      ends_apart <- (key$theta[n] - key$theta[1]) %% (2 * pi)
      if (n > 0 && ends_apart < 0.05 && !is.null(key$.label)) {
        if (is.expression(key$.label)) {
          combined <- substitute(
            paste(a, "/", b),
            list(a = key$.label[[1]], b = key$.label[[n]])
          )
        } else {
          combined <- paste(key$.label[1], key$.label[n], sep = "/")
        }
        key$.label[[n]] <- combined
        key <- vec_slice(key, -1)
      }
    }

    params$key <- key
    params
  },

  setup_params = function(params) {
    # Theta axis doesn't need to setup any position specific parameters.
    params
  },

  setup_elements = function(params, elements, theme) {

    axis_elem <- c("line", "text", "ticks", "minor", "major_length", "minor_length")
    is_char <- vapply(elements[axis_elem], is.character, logical(1))
    axis_elem <- axis_elem[is_char]

    aes <- switch(
      params$position,
      theta     = "x.bottom",
      theta.sec = "x.top",
      paste0(params$aesthetic, ".", params$position)
    )

    elements[axis_elem] <- lapply(
      paste(unlist(elements[axis_elem]), aes, sep = "."),
      calc_element, theme = theme
    )

    # Offset distance from axis arc to text positions
    if (!params$minor.ticks) {
      elements$minor_length <- unit(0, "pt")
    }

    offset <- max(unit(0, "pt"), elements$major_length, elements$minor_length)
    elements$offset <- offset + max(elements$text$margin %||% unit(0, "pt"))
    elements
  },

  override_elements = function(params, elements, theme) {
    # We don't override any label angles/hjust/vjust because these depend on
    # theta of label.
    elements
  },

  build_labels = function(key, elements, params) {

    if (inherits(elements$text, "element_blank")) {
      return(zeroGrob())
    }

    key <- vec_slice(key, !vec_detect_missing(key$.label %||% NA))

    # Early exit if drawing no labels
    labels <- key$.label
    if (length(labels) < 1) {
      return(zeroGrob())
    }

    # Resolve text angle
    if (is.waive(params$angle) || is.null(params$angle)) {
      angle <- elements$text$angle
    } else {
      angle <- flip_text_angle(params$angle - rad2deg(key$theta))
    }
    # Text angle in radians
    rad <- deg2rad(angle)
    # Position angle in radians
    theta <- key$theta

    # Offset distance to displace text away from outer circle line
    xoffset <- elements$offset * sin(theta)
    yoffset <- elements$offset * cos(theta)

    # Note that element_grob expects 1 angle for *all* labels, so we're
    # rendering one grob per label to propagate angle properly
    element_grob(
      elements$text,
      label = labels,
      x     = unit(key$x, "npc") + xoffset,
      y     = unit(key$y, "npc") + yoffset,
      hjust = 0.5 - sin(theta + rad) / 2,
      vjust = 0.5 - cos(theta + rad) / 2,
      angle = angle
    )
  },

  build_ticks = function(key, elements, params, position = params$position) {

    major <- theta_tickmarks(
      vec_slice(key, (key$.type %||% "major") == "major"),
      elements$ticks, elements$major_length
    )
    minor <- theta_tickmarks(
      vec_slice(key, (key$.type %||% "major") == "minor"),
      elements$minor, elements$minor_length
    )

    grobTree(major, minor, name = "ticks")
  },

  measure_grobs = function(grobs, params, elements) {
    # As this guide is expected to be placed in the interior of coord_radial,
    # we don't need to measure grob sizes nor arrange the layout.
    # There is a fallback in `$assemble_drawing()` that takes care of this
    # for non-polar coordinates.
    NULL
  },

  arrange_layout = function(key, sizes, params) {
    NULL
  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {
    if (params$position %in% c("theta", "theta.sec")) {
      return(inject(grobTree(!!!grobs)))
    }

    # As a fallback, we adjust the viewport to act like regular axes.
    if (params$position %in% c("top", "bottom")) {
      height <- sum(
        elements$offset,
        unit(max(height_cm(grobs$labels$children)), "cm")
      )
      vp <- viewport(
        y = unit(as.numeric(params$position == "bottom"), "npc"),
        height = height, width = unit(1, "npc"),
        just = opposite_position(params$position)
      )
    } else {
      width <- sum(
        elements$offset,
        unit(max(width_cm(grobs$labels$children)), "cm")
      )
      vp <- viewport(
        x = unit(as.numeric(params$position == "left"), "npc"),
        height = unit(1, "npc"), width = width,
        just = opposite_position(params$position)
      )
    }

    absoluteGrob(
      inject(gList(!!!grobs)),
      width  = vp$width,
      height = vp$height,
      vp = vp
    )
  }
)

theta_tickmarks <- function(key, element, length) {
  n_breaks <- nrow(key)
  if (n_breaks < 1 || inherits(element, "element_blank")) {
    return(zeroGrob())
  }

  length <- rep(length, length.out = n_breaks * 2)
  angle  <- rep(key$theta, each = 2)
  x      <- rep(key$x,     each = 2)
  y      <- rep(key$y,     each = 2)
  length <- rep(c(0, 1),  times = n_breaks) * length

  minor <- element_grob(
    element,
    x = unit(x, "npc") + sin(angle) * length,
    y = unit(y, "npc") + cos(angle) * length,
    id.lengths = rep(2, n_breaks)
  )
}
