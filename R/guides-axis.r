
#' Grob for axes
#'
#' @param break_position position of ticks
#' @param break_labels labels at ticks
#' @param axis_position position of axis (top, bottom, left or right)
#' @param theme A [theme()] object
#'
#' @noRd
#'
draw_axis <- function(break_positions, break_labels, axis_position, theme) {

  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if(axis_position %in% c("top", "bottom")) "x" else "y"

  is_vertical <- axis_position %in% c("left",  "right")
  is_second <- axis_position %in% c("right", "top") # refers to positive npc coordinates
  is_first_gtable <- axis_position %in% c("left", "top") # refers to position in gtable
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
  tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length <- calc_element(tick_length_element_name, theme)
  label_element <- calc_element(label_element_name, theme)

  if (is_vertical) {
    position_dim <- "y"
    non_position_dim <- "x"
    position_size <- "height"
    non_position_size <- "width"
    label_margin_name <- "margin_x"
    gtable_element <- gtable_row
    measure_gtable <- gtable_width
    measure_labels <- grobWidth
  } else {
    position_dim <- "x"
    non_position_dim <- "y"
    position_size <- "width"
    non_position_size <- "height"
    label_margin_name <- "margin_y"
    gtable_element <- gtable_col
    measure_gtable <- gtable_height
    measure_labels <- grobHeight
  }

  if (is_second) {
    tick_direction <- 1
    non_position_panel <- unit(0, "npc")
    tick_coordinate_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- unit(1, "npc")
    tick_coordinate_order <- c(1, 2)
  }

  if (is_first_gtable) {
    table_order <- c("labels", "ticks")
  } else {
    table_order <- c("ticks", "labels")
  }

  # draw elements
  line_coords <- list(
    position = unit(c(0, 1), "npc"),
    non_position = unit.c(non_position_panel, non_position_panel)
  )
  names(line_coords) <- c(position_dim, non_position_dim)
  line_grob <- do.call(element_grob, c(list(line_element), line_coords))

  if (n_breaks == 0) {
    return(
      absoluteGrob(
        gList(line_grob),
        width = grobWidth(line_grob),
        height = grobHeight(line_grob)
      )
    )
  }

  label_coords <- list(
    position = unit(break_positions, "native"),
    label = break_labels,
    margin = TRUE
  )

  tick_coords <- list(
    position = rep(label_coords$position, each = 2),
    non_position = rep(
      unit.c(non_position_panel + (tick_direction * tick_length), non_position_panel)[tick_coordinate_order],
      times = n_breaks
    ),
    id.lengths = rep(2, times = n_breaks)
  )

  names(label_coords) <- c(position_dim, "label", label_margin_name)
  names(tick_coords) <- c(position_dim, non_position_dim, "id.lengths")

  grobs <- list(
    line = line_grob,
    labels = do.call(element_grob, c(list(label_element), label_coords)),
    ticks = do.call(element_grob, c(list(tick_element), tick_coords))
  )

  # assemble elements
  gt_element_order <- match(table_order, c("labels", "ticks"))
  gt_dims <- list(
    dims = unit.c(measure_labels(grobs$labels), tick_length),
    dim = unit(1, "npc")
  )
  gt_dims$dims <- gt_dims$dims[gt_element_order]
  names(gt_dims) <- c(paste0(non_position_size, "s"), position_size)

  gt <- do.call(
    gtable_element,
    c(list(name = "axis", grobs = grobs[table_order]), gt_dims)
  )

  justvp_args <- list(
    non_position_dim = non_position_panel,
    just = axis_position_opposite,
    non_position_size = measure_gtable(gt)
  )
  names(justvp_args) <- c(non_position_dim, "just", non_position_size)

  justvp <- do.call(viewport, justvp_args)

  absoluteGrob(
    gList(grobs$line, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}
