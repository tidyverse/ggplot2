
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
#' @param n_dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#' @param align_ends Ensure end labels are within the bounds of the axis
#'   space.
#'
#' @noRd
#'
draw_axis <- function(break_positions, break_labels, axis_position, theme,
                      check.overlap = FALSE, angle = NULL, n_dodge = 1,
                      align_ends = FALSE) {

  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"

  # resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
  tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length <- calc_element(tick_length_element_name, theme)
  label_element <- calc_element(label_element_name, theme)

  # override label element parameters for rotation
  if (inherits(label_element, "element_text")) {
    label_element <- merge_element(
      axis_label_element_overrides(axis_position, angle),
      label_element
    )
  }

  # conditionally set parameters that depend on axis orientation
  is_vertical <- axis_position %in% c("left",  "right")

  position_dim <- if (is_vertical) "y" else "x"
  non_position_dim <- if (is_vertical) "x" else "y"
  position_size <- if (is_vertical) "height" else "width"
  non_position_size <- if (is_vertical) "width" else "height"
  gtable_element <- if (is_vertical) gtable_row else gtable_col
  measure_gtable <- if (is_vertical) gtable_width else gtable_height
  measure_labels_non_pos <- if (is_vertical) grobWidth else grobHeight

  # conditionally set parameters that depend on which side of the panel
  # the axis is on
  is_second <- axis_position %in% c("right", "top")

  tick_direction <- if (is_second) 1 else -1
  non_position_panel <- if (is_second) unit(0, "npc") else unit(1, "npc")
  tick_coordinate_order <- if (is_second) c(2, 1) else c(1, 2)

  # conditionally set the gtable ordering
  labels_first_gtable <- axis_position %in% c("left", "top") # refers to position in gtable

  # set common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # draw elements
  line_grob <- exec(
    element_grob, line_element,
    !!position_dim := unit(c(0, 1), "npc"),
    !!non_position_dim := unit.c(non_position_panel, non_position_panel)
  )

  if (n_breaks == 0) {
    return(
      absoluteGrob(
        gList(line_grob),
        width = grobWidth(line_grob),
        height = grobHeight(line_grob)
      )
    )
  }

  # break_labels can be a list() of language objects
  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))) {
      break_labels <- do.call(expression, break_labels)
    } else {
      break_labels <- unlist(break_labels)
    }
  }

  # calculate multiple rows/columns of labels (which is usually 1)
  dodge_pos <- rep(seq_len(n_dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)

  label_grobs <- lapply(dodge_indices, function(indices) {
    draw_axis_labels(
      break_positions = break_positions[indices],
      break_labels = break_labels[indices],
      label_element = label_element,
      is_vertical = is_vertical,
      check.overlap = check.overlap,
      align_ends = align_ends
    )
  })

  ticks_grob <- exec(
    element_grob, tick_element,
    !!position_dim := rep(unit(break_positions, "native"), each = 2),
    !!non_position_dim := rep(
      unit.c(non_position_panel + (tick_direction * tick_length), non_position_panel)[tick_coordinate_order],
      times = n_breaks
    ),
    id.lengths = rep(2, times = n_breaks)
  )

  # create gtable
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(unit.c, lapply(label_grobs, measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- unit.c(tick_length, label_dims)

  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- exec(
    gtable_element,
    name = "axis",
    grobs = grobs,
    !!non_position_sizes := grob_dims,
    !!position_size := unit(1, "npc")
  )

  # create viewport
  justvp <- exec(
    viewport,
    !!non_position_dim := non_position_panel,
    !!non_position_size := measure_gtable(gt),
    just = axis_position_opposite
  )

  absoluteGrob(
    gList(line_grob, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}

draw_axis_labels <- function(break_positions, break_labels, label_element, is_vertical,
                             check.overlap = FALSE, align_ends = FALSE) {

  position_dim <- if (is_vertical) "y" else "x"
  label_margin_name <- if (is_vertical) "margin_x" else "margin_y"
  measure_labels_pos <- if (is_vertical) grobHeight else grobWidth

  n_breaks <- length(break_positions)
  break_positions <- unit(break_positions, "native")

  if (align_ends) {

    # getting the alignment code to work properly for text that isn't parallel
    # to the axis is a lot of effort for little gain
    if (is_vertical && abs(label_element$angle) != 90) {
      stop("Cannot align end labels for vertical axis when `angle` is not 90 or -90", call. = FALSE)
    }
    if (!is_vertical && label_element$angle != 0) {
      stop("Cannot align end labels for horizontal axis when `angle` is not 0", call. = FALSE)
    }

    first_label <- exec(element_grob, label_element, label = break_labels[1])
    last_label <- exec(element_grob, label_element, label = break_labels[n_breaks])

    first_label_dim <- measure_labels_pos(first_label)
    last_label_dim <- measure_labels_pos(last_label)
    just <- label_element$hjust

    break_positions[1] <- max(
      break_positions[1],
      unit(0, "npc") + first_label_dim * just
    )

    break_positions[n_breaks] <- min(
      break_positions[n_breaks],
      unit(1, "npc") - last_label_dim * (1 - just)
    )
  }

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
  if (is.null(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }

  # it is not worth the effort to align upside-down labels properly
  if (angle > 90 || angle < -90) {
    stop("`angle` must be between 90 and -90", call. = FALSE)
  }

  if (axis_position == "bottom") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 1
    )
  } else if (axis_position == "left") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 1,
      vjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
    )
  } else if (axis_position == "top") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 0
    )
  } else if (axis_position == "right") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 0,
      vjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
    )
  } else {
    stop("Unrecognized position: '", axis_position, "'", call. = FALSE)
  }
}
