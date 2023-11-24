
#' Axis guide
#'
#' Axis guides are the visual representation of position scales like those
#' created with [scale_(x|y)_continuous()][scale_x_continuous()] and
#' [scale_(x|y)_discrete()][scale_x_discrete()].
#'
#' @inheritParams guide_legend
#' @param check.overlap silently remove overlapping labels,
#'   (recursively) prioritizing the first, last, and middle labels.
#' @param angle Compared to setting the angle in [theme()] / [element_text()],
#'   this also uses some heuristics to automatically pick the `hjust` and `vjust` that
#'   you probably want. Can be one of the following:
#'   * `NULL` to take the angles and `hjust`/`vjust` directly from the theme.
#'   * `waiver()` to allow reasonable defaults in special cases.
#'   *  A number representing the text angle in degrees.
#' @param n.dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#' @param minor.ticks Whether to draw the minor ticks (`TRUE`) or not draw
#'   minor ticks (`FALSE`, default).
#' @param cap A `character` to cut the axis line back to the last breaks. Can
#'   be `"none"` (default) to draw the axis line along the whole panel, or
#'   `"upper"` and `"lower"` to draw the axis to the upper or lower break, or
#'   `"both"` to only draw the line in between the most extreme breaks. `TRUE`
#'   and `FALSE` are shorthand for `"both"` and `"none"` respectively.
#' @param order A positive `integer` of length 1 that specifies the order of
#'   this guide among multiple guides. This controls in which order guides are
#'   merged if there are multiple guides for the same position. If 0 (default),
#'   the order is determined by a secret algorithm.
#' @param position Where this guide should be drawn: one of top, bottom,
#'   left, or right.
#'
#' @export
#'
#' @examples
#' # plot with overlapping text
#' p <- ggplot(mpg, aes(cty * 100, hwy * 100)) +
#'   geom_point() +
#'   facet_wrap(vars(class))
#'
#' # axis guides can be customized in the scale_* functions or
#' # using guides()
#' p + scale_x_continuous(guide = guide_axis(n.dodge = 2))
#' p + guides(x = guide_axis(angle = 90))
#'
#' # can also be used to add a duplicate guide
#' p + guides(x = guide_axis(n.dodge = 2), y.sec = guide_axis())
guide_axis <- function(title = waiver(), check.overlap = FALSE, angle = waiver(),
                       n.dodge = 1, minor.ticks = FALSE, cap = "none",
                       order = 0, position = waiver()) {
  check_bool(minor.ticks)
  if (is.logical(cap)) {
    check_bool(cap)
    cap <- if (cap) "both" else "none"
  }
  cap <- arg_match0(cap, c("none", "both", "upper", "lower"))

  new_guide(
    title = title,

    # customisations
    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,
    minor.ticks = minor.ticks,
    cap = cap,

    # parameter
    available_aes = c("x", "y", "r"),

    # general
    order = order,
    position = position,
    name = "axis",
    super = GuideAxis
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideAxis <- ggproto(
  "GuideAxis", Guide,

  params = list(
    title     = waiver(),
    name      = "axis",
    hash      = character(),
    position  = waiver(),
    direction = NULL,
    angle     = NULL,
    n.dodge   = 1,
    minor.ticks = FALSE,
    cap       = "none",
    order     = 0,
    check.overlap = FALSE
  ),

  available_aes = c("x", "y"),

  hashables = exprs(title, key$.value, key$.label, name),

  elements = list(
    line  = "axis.line",
    text  = "axis.text",
    ticks = "axis.ticks",
    minor = "axis.minor.ticks",
    major_length = "axis.ticks.length",
    minor_length = "axis.minor.ticks.length"
  ),

  extract_key = function(scale, aesthetic, minor.ticks = FALSE, ...) {
    major <- Guide$extract_key(scale, aesthetic, ...)
    if (!minor.ticks) {
      return(major)
    }

    minor_breaks <- scale$get_breaks_minor()
    minor_breaks <- setdiff(minor_breaks, major$.value)
    minor_breaks <- minor_breaks[is.finite(minor_breaks)]

    if (length(minor_breaks) < 1) {
      return(major)
    }

    minor <- data_frame0(!!aesthetic := scale$map(minor_breaks))
    minor$.value <- minor_breaks
    minor$.type <- "minor"

    if (nrow(major) > 0) {
      major$.type <- "major"
      vec_rbind(major, minor)
    } else {
      minor
    }
  },

  extract_params = function(scale, params, ...) {
    params$name <- paste0(params$name, "_", params$aesthetic)
    params
  },

  extract_decor = function(scale, aesthetic, position, key, cap = "none", ...) {

    value <- c(-Inf, Inf)
    if (cap %in% c("both", "upper")) {
      value[2] <- max(key[[aesthetic]])
    }
    if (cap %in% c("both", "lower")) {
      value[1] <- min(key[[aesthetic]])
    }

    opposite <- setdiff(c("x", "y"), aesthetic)
    opposite_value <- if (position %in% c("top", "right")) -Inf else Inf

    data_frame(
      !!aesthetic := value,
      !!opposite  := opposite_value
    )
  },

  transform = function(self, params, coord, panel_params) {
    key <- params$key
    position <- params$position

    if (is.null(position) || nrow(key) == 0) {
      return(params)
    }

    aesthetics <- names(key)[!grepl("^\\.", names(key))]
    if (!all(c("x", "y") %in% aesthetics)) {
      other_aesthetic <- setdiff(c("x", "y"), aesthetics)
      override_value <- if (position %in% c("bottom", "left")) -Inf else Inf
      key[[other_aesthetic]] <- override_value
    }
    key <- coord$transform(key, panel_params)
    params$key <- key

    params$decor <- coord_munch(coord, params$decor, panel_params)

    if (!coord$is_linear()) {
      # For non-linear coords, we hardcode the opposite position
      params$decor$x <- switch(position, left = 1, right = 0, params$decor$x)
      params$decor$y <- switch(position, top = 0, bottom = 1, params$decor$y)
    }

    # Ported over from `warn_for_position_guide`
    # This is trying to catch when a user specifies a position perpendicular
    # to the direction of the axis (e.g., a "y" axis on "top").
    # The strategy is to check that two or more unique breaks are mapped
    # to the same value along the axis.
    breaks_are_unique <- !duplicated(key$.value)
    if (empty(key) || sum(breaks_are_unique) == 1) {
      return(params)
    }

    if (position %in% c("top", "bottom")) {
      position_aes <- "x"
    } else if (position %in% c("left", "right")) {
      position_aes <- "y"
    } else {
      return(params)
    }

    if (length(unique(key[[position_aes]][breaks_are_unique])) == 1) {
      cli::cli_warn(c(
        "Position guide is perpendicular to the intended axis.",
        "i" = "Did you mean to specify a different guide {.arg position}?"
      ))
    }

    return(params)
  },

  merge = function(self, params, new_guide, new_params) {
    if (!inherits(new_guide, "GuideNone")) {
      cli::cli_warn(c(
        "{.fn {snake_class(self)}}: Discarding guide on merge.",
        "i" = "Do you have more than one guide with the same {.arg position}?"
      ))
    }
    return(list(guide = self, params = params))
  },

  setup_elements = function(params, elements, theme) {
    axis_elem <- c("line", "text", "ticks", "minor", "major_length", "minor_length")
    is_char  <- vapply(elements[axis_elem], is.character, logical(1))
    axis_elem <- axis_elem[is_char]
    elements[axis_elem] <- lapply(
      paste(
        unlist(elements[axis_elem]),
        params$aes, params$position, sep = "."
      ),
      calc_element, theme = theme
    )
    elements
  },

  override_elements = function(params, elements, theme) {
    label <- elements$text
    if (!inherits(label, "element_text")) {
      return(elements)
    }
    label_overrides <- axis_label_element_overrides(
      params$position, params$angle
    )
    # label_overrides is an element_text, but label_element may not be;
    # to merge the two elements, we just copy angle, hjust, and vjust
    # unless their values are NULL
    label$angle <- label_overrides$angle %||% label$angle
    label$hjust <- label_overrides$hjust %||% label$hjust
    label$vjust <- label_overrides$vjust %||% label$vjust

    elements$text <- label
    return(elements)
  },

  setup_params = function(params) {
    position  <- arg_match0(params$position, .trbl)
    direction <- if (position %in% c("left", "right")) {
      "vertical"
    } else {
      "horizontal"
    }

    new_params <- c("aes", "orth_aes", "para_sizes", "orth_size", "orth_sizes",
                    "vertical", "measure_gtable", "measure_text")
    if (direction == "vertical") {
      params[new_params] <- list(
        "y", "x", "heights", "width", "widths",
        TRUE, gtable_width, width_cm
      )
    } else {
      params[new_params] <- list(
        "x", "y", "widths", "height", "heights",
        FALSE, gtable_height, height_cm
      )
    }

    new_params <- list(
      opposite  = opposite_position(position),
      secondary = position %in% c("top", "right"),
      lab_first = position %in% c("top", "left"),
      orth_side = if (position %in% c("top", "right")) 0 else 1,
      direction = direction,
      position  = position
    )
    c(params, new_params)
  },

  build_title = function(label, elements, params) {
    zeroGrob()
  },

  # The decor in the axis guide is the axis line
  build_decor = function(decor, grobs, elements, params) {
    if (empty(decor)) {
      return(zeroGrob())
    }
    element_grob(
      elements$line,
      x = unit(decor$x, "npc"),
      y = unit(decor$y, "npc")
    )
  },

  build_ticks = function(key, elements, params, position = params$opposite) {

    major <- Guide$build_ticks(
      vec_slice(key, (key$.type %||% "major") == "major"),
      elements$ticks, params, position,
      elements$major_length
    )

    if (!params$minor.ticks) {
      return(major)
    }

    minor <- Guide$build_ticks(
      vec_slice(key, (key$.type %||% "major") == "minor"),
      elements$minor, params, position,
      elements$minor_length
    )
    grobTree(major, minor, name = "ticks")
  },

  build_labels = function(key, elements, params) {

    if (".type" %in% names(key)) {
      key <- vec_slice(key, key$.type == "major")
    }

    labels   <- validate_labels(key$.label)
    n_labels <- length(labels)

    if (n_labels < 1) {
      return(list(zeroGrob()))
    }

    pos <- key[[params$aes]]

    dodge_pos     <- rep(seq_len(params$n.dodge %||% 1), length.out = n_labels)
    dodge_indices <- unname(split(seq_len(n_labels), dodge_pos))

    lapply(dodge_indices, function(indices) {
      draw_axis_labels(
        break_positions = pos[indices],
        break_labels    = labels[indices],
        label_element   = elements$text,
        is_vertical     = params$vertical,
        check.overlap   = params$check.overlap %||% FALSE
      )
    })
  },

  measure_grobs = function(grobs, params, elements) {

    # Below, we include a spacer measurement. This measurement is used
    # to offset subsequent rows/columns in the gtable in case the tick length is
    # negative. This causes the text to align nicely at panel borders.
    # In case tick length is positive, this will just be a 0-size empty row
    # or column.

    measure <- params$measure_text

    # Ticks
    major_cm <- convertUnit(elements$major_length, "cm", valueOnly = TRUE)
    range <- range(0, major_cm)
    if (params$minor.ticks && !inherits(elements$minor, "element_blank")) {
      minor_cm <- convertUnit(elements$minor_length, "cm", valueOnly = TRUE)
      range <- range(range, minor_cm)
    }

    length <- unit(range[2], "cm")
    spacer <- max(unit(0, "pt"), unit(-1 * diff(range), "cm"))

    # Text
    labels <- unit(measure(grobs$labels), "cm")
    title  <- unit(measure(grobs$title), "cm")

    sizes <- unit.c(length, spacer, labels, title)
    if (params$lab_first) {
      sizes <- rev(sizes)
    }
    sizes
  },

  arrange_layout = function(key, sizes, params) {

    layout <- seq_along(sizes)

    if (params$lab_first) {
      layout <- rev(layout)
    }
    # Set gap for spacer
    layout <- layout[-2]

    layout <- list(1, -1, layout, layout)
    nms <- if (params$vertical) c("t", "b", "l", "r") else c("l", "r", "t", "b")
    setNames(layout, nms)
  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {

    axis_line <- grobs$decor

    # Unlist the 'label' grobs
    z <- if (params$position == "left") c(2, 1, 3) else 1:3
    z <- rep(z, c(1, length(grobs$labels), 1))
    grobs  <- c(list(grobs$ticks), grobs$labels, list(grobs$title))

    # Initialise empty gtable
    gt <- exec(
      gtable,
      !!params$orth_sizes := sizes,
      !!params$para_sizes := unit(1, "npc"),
      name = "axis"
    )

    # Add grobs
    gt <- gtable_add_grob(
      gt, grobs,
      t = layout$t, b = layout$b, l = layout$l, r = layout$r,
      clip = "off", z = z
    )

    # Set justification viewport
    vp <- exec(
      viewport,
      !!params$orth_aes := unit(params$orth_side, "npc"),
      !!params$orth_size := params$measure_gtable(gt),
      just = params$opposite
    )

    # Assemble with axis line
    absoluteGrob(
      gList(axis_line, gt),
      width  = gtable_width(gt),
      height = gtable_height(gt),
      vp = vp
    )
  },

  draw_early_exit = function(self, params, elements) {
    line <- self$build_decor(decor = params$decor, elements = elements,
                             params = params)
    absoluteGrob(
      gList(line),
      width  = grobWidth(line),
      height = grobHeight(line)
    )
  }
)

# TODO: If #3972 gets implemented, reconsider the usefulness of this function.
# We still need the `draw_axis` function because most coords other than
# `coord_cartesian()` ignore guides. See #3972

#' Grob for axes
#'
#' @param break_position position of ticks
#' @param break_labels labels at ticks
#' @param axis_position position of axis (top, bottom, left or right)
#' @param theme A complete [theme()] object
#' @param check.overlap silently remove overlapping labels,
#'   (recursively) prioritizing the first, last, and middle labels.
#' @param angle Compared to setting the angle in [theme()] / [element_text()],
#'   this also uses some heuristics to automatically pick the `hjust` and `vjust` that
#'   you probably want.
#' @param n.dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#'
#' @noRd
#'
draw_axis <- function(break_positions, break_labels, axis_position, theme,
                      check.overlap = FALSE, angle = NULL, n.dodge = 1) {
  guide <- guide_axis(check.overlap = check.overlap,
                      angle = angle,
                      n.dodge = n.dodge,
                      position = axis_position)
  params <- guide$params
  aes <- if (axis_position %in% c("top", "bottom")) "x" else "y"
  opp <- setdiff(c("x", "y"), aes)
  opp_value <- if (axis_position %in% c("top", "right")) 0 else 1
  key <- data_frame0(
    !!aes := break_positions,
    .value = break_positions,
    .label = break_labels
  )
  params$key <- key
  params$decor <- data_frame0(
    !!aes := c(0, 1),
    !!opp := opp_value
  )
  guide$draw(theme, params = params)
}

draw_axis_labels <- function(break_positions, break_labels, label_element, is_vertical,
                             check.overlap = FALSE) {

  position_dim <- if (is_vertical) "y" else "x"
  label_margin_name <- if (is_vertical) "margin_x" else "margin_y"

  n_breaks <- length(break_positions)
  break_positions <- unit(break_positions, "native")

  if (check.overlap) {
    priority <- axis_label_priority(n_breaks)
    break_labels <- break_labels[priority]
    break_positions <- break_positions[priority]
  }

  labels_grob <- exec(
    element_grob, label_element,
    !!position_dim := break_positions,
    !!label_margin_name := TRUE,
    label = break_labels,
    check.overlap = check.overlap
  )
}

#' Determine the label priority for a given number of labels
#'
#' @param n The number of labels
#'
#' @return The vector `seq_len(n)` arranged such that the
#'   first, last, and middle elements are recursively
#'   placed at the beginning of the vector.
#' @noRd
#'
axis_label_priority <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }

  c(1, n, axis_label_priority_between(1, n))
}

axis_label_priority_between <- function(x, y) {
  n <- y - x + 1
  if (n <= 2) {
    return(numeric(0))
  }

  mid <- x - 1 + (n + 1) %/% 2
  c(
    mid,
    axis_label_priority_between(x, mid),
    axis_label_priority_between(mid, y)
  )
}

#' Override axis text angle and alignment
#'
#' @param axis_position One of bottom, left, top, or right
#' @param angle The text angle, or NULL to override nothing
#'
#' @return An [element_text()] that contains parameters that should be
#'   overridden from the user- or theme-supplied element.
#' @noRd
#'
axis_label_element_overrides <- function(axis_position, angle = NULL) {

  if (is.null(angle) || is.waive(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }

  check_number_decimal(angle)
  angle <- angle %% 360

  if (axis_position == "bottom") {

    hjust = if (angle %in% c(0, 180))  0.5 else if (angle < 180) 1 else 0
    vjust = if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 0 else 1

  } else if (axis_position == "left") {

    hjust = if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 0 else 1
    vjust = if (angle %in% c(0, 180))  0.5 else if (angle < 180) 0 else 1

  } else if (axis_position == "top") {

    hjust = if (angle %in% c(0, 180))  0.5 else if (angle < 180) 0 else 1
    vjust = if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 1 else 0

  } else if (axis_position == "right") {

    hjust = if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 1 else 0
    vjust = if (angle %in% c(0, 180))  0.5 else if (angle < 180) 1 else 0

  } else {

    cli::cli_abort(c(
      "Unrecognized {.arg axis_position}: {.val {axis_position}}",
      "i" = "Use one of {.val top}, {.val bottom}, {.val left} or {.val right}"
    ))

  }

  element_text(angle = angle, hjust = hjust, vjust = vjust)
}
