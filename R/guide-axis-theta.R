#' @include guide-axis.R
NULL

#' Angle axis guide
#'
#' This is a specialised guide used in `coord_polar2()` to represent the theta
#' position scale.
#'
#' @inheritParams guide_axis
#' @param major.length,minor.length A `numeric` of length 1 giving the length of
#'   major and minor ticks relative to the theme's setting.
#' @param minor.ticks A theme element inheriting from `element_line` or
#'   `element_blank` for drawing minor ticks. Alternatively, a `logical` of
#'   length 1 as shorthand for `element_line()` (`TRUE`) or `element_blank`
#'   (`FALSE`). `minor.ticks = element_line(...)` can be used to style the
#'   minor ticks.
#'
#' @note
#' The axis labels in this guide are insensitive to `hjust` and `vjust`
#' settings. The distance from the tick marks to the labels is determined by
#' the largest `margin` size set in the theme.
#'
#' @export
#'
#' @examples
#' # A basic polar plot
#' p <- ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   coord_polar2()
#'
#' # The `angle` argument can be used to set relative angles
#' p + guides(theta = guide_axis_theta(angle = 0))
#'
#' # Minor ticks can be activated by providing a line element
#' p + guides(theta = guide_axis_theta(minor.ticks = element_line()))
guide_axis_theta <- function(title = waiver(), angle = waiver(),
                             cap = "none", order = 0,
                             major.length = 1, minor.length = 0.75,
                             minor.ticks = element_blank(),
                             position = waiver()) {

  if (is.logical(cap)) {
    check_bool(cap)
    cap <- if (cap) "both" else "none"
  }
  cap <- arg_match0(cap, c("none", "both", "upper", "lower"))

  if (is.logical(minor.ticks)) {
    check_bool(minor.ticks)
    minor.ticks <- if (minor.ticks) element_line() else element_blank()
  }
  check_inherits(minor.ticks, c("element_line", "element_blank"))
  if (inherits(minor.ticks, "element_blank")) {
    minor.length <- 0
  }

  new_guide(
    title = title,

    # customisations
    angle = angle,
    cap = cap,
    major.length = major.length,
    minor.length = minor.length,
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

  # TODO: delete if minor ticks PR (#5287) gets merged
  params = c(GuideAxis$params, list(
    major.length = 1,
    minor.length = 0.75,
    minor.ticks  = NULL
  )),

  # TODO: delete if minor ticks PR (#5287) gets merged
  extract_key = function(scale, aesthetic, minor.ticks, ...) {
    major <- Guide$extract_key(scale, aesthetic, ...)
    if (is.expression(major$.label)) {
      major$.label <- as.list(major$.label)
    }
    if (inherits(minor.ticks, "element_blank")) {
      return(major)
    }
    if (!is.null(major)) {
      major$.type <- "major"
    }
    minor <- setdiff(scale$get_breaks_minor(), major$.value)
    new_scale <- ggproto(NULL, scale, breaks = minor, get_labels = function(...) NULL)
    minor <- Guide$extract_key(new_scale, aesthetic, ...)
    minor$.type <- "minor"
    vec_rbind(major, minor)
  },

  extract_decor = function(scale, aesthetic, key, cap = "none", ...) {
    # We put position = "left" to get `Inf` on the opposite aesthetic
    GuideAxis$extract_decor(
      scale = scale, aesthetic = aesthetic,
      position = "left", key = key, cap = cap
    )
  },

  transform = function(params, coord, panel_params) {

    if (params$position != "theta") {
      cli::cli_warn(c(paste0(
        "{.fn guide_axis_theta} cannot be used for the ",
        "{.field {params$position}} position."
      ), i = "It requires the position to be {.field theta}."))
      return(NULL)
    }

    opposite <- setdiff(c("x", "y"), params$aesthetic)
    params$key[[opposite]] <- Inf
    params <- GuideAxis$transform(params, coord, panel_params)

    key <- params$key
    n <- nrow(key)

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

    params$key <- key
    params
  },

  setup_params = function(params) {
    params
  },

  setup_elements = function(params, elements, theme) {
    axis_elem <- c("line", "text", "ticks", "ticks_length")
    is_char <- vapply(elements[axis_elem], is.character, logical(1))
    axis_elem <- axis_elem[is_char]
    elements[axis_elem] <- lapply(
      paste(
        unlist(elements[axis_elem]),
        params$aes, sep = "."
      ),
      calc_element, theme = theme
    )
    elements$minor_ticks <- combine_elements(params$minor.ticks, elements$ticks)
    elements
  },

  override_elements = function(params, elements, theme) {
    return(elements)
  },

  build_labels = function(key, elements, params) {

    key <- vec_slice(key, !vec_detect_missing(key$.label %||% NA))

    # Early exit if drawing no labels
    labels <- key$.label
    if (length(labels) < 1) {
      return(list(zeroGrob()))
    }

    # Resolve text angle
    if (is.waive(params$angle) || is.null(params$angle)) {
      angle <- elements$text$angle
    } else {
      angle <- (360 - key$theta * 180 / pi + params$angle) %% 360
      flip <- angle > 90 & angle < 270
      angle[flip] <- angle[flip] + 180
    }
    # Text angle in radians
    rad <- angle * pi / 180
    # Position angle in radians
    theta <- key$theta

    # Offset distance to displace text away from outer circle line
    offset <- max(0, params$major.length, params$minor.length)
    offset  <- max(elements$ticks_length * offset, unit(0, "pt")) +
      max(elements$text$margin)
    xoffset <- offset * sin(theta)
    yoffset <- offset * cos(theta)

    # Note that element_grob expects 1 angle for *all* labels, so we're
    # rendering one grob per label to propagate angle properly
    do.call(grobTree, Map(
      element_grob,
      label = labels,
      x = unit(key$x, "npc") + xoffset,
      y = unit(key$y, "npc") + yoffset,
      hjust = 0.5 - sin(theta + rad) / 2,
      vjust = 0.5 - cos(theta + rad) / 2,
      angle = angle,
      MoreArgs = list(element = elements$text)
    ))
  },

  build_ticks = function(key, elements, params, position = params$position) {

    if (".type" %in% names(key)) {
      major <- vec_slice(key, key$.type == "major")
      minor <- vec_slice(key, key$.type == "minor")
    } else {
      major <- key
      minor <- NULL
    }

    n_breaks <- nrow(major)

    tick_len <- elements$ticks_length * params$major.length
    tick_len <- rep(tick_len, length.out = n_breaks * 2)

    angle <- rep(major$theta, each = 2)
    x     <- rep(major$x, each = 2)
    y     <- rep(major$y, each = 2)
    end   <- rep(c(0, 1), n_breaks)

    major <- element_grob(
      elements$ticks,
      x = unit(x, "npc") + sin(angle) * end * tick_len,
      y = unit(y, "npc") + cos(angle) * end * tick_len,
      id.lengths = rep(2, n_breaks)
    )

    if (empty(minor) || inherits(elements$minor_ticks, "element_blank")) {
      return(major)
    }

    n_breaks <- nrow(minor)

    tick_len <- elements$ticks_length * params$minor.length
    tick_len <- rep(tick_len, length.out = n_breaks * 2)

    angle <- rep(minor$theta, each = 2)
    x     <- rep(minor$x, each = 2)
    y     <- rep(minor$y, each = 2)
    end   <- rep(c(0, 1), n_breaks)

    minor <- element_grob(
      elements$minor_ticks,
      x = unit(x, "npc") + sin(angle) * end * tick_len,
      y = unit(y, "npc") + cos(angle) * end * tick_len,
      id.lengths = rep(2, n_breaks)
    )

    grobTree(major, minor, name = "ticks")
  },

  measure_grobs = function(grobs, params, elements) {
    return(invisible())
  },

  arrange_layout = function(key, sizes, params) {
    return(invisible())
  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {
    do.call(grobTree, grobs)
  }

)

