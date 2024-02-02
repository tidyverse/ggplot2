#' @include facet-.R
NULL

#' Wrap a 1d ribbon of panels into 2d
#'
#' `facet_wrap()` wraps a 1d sequence of panels into 2d. This is generally
#' a better use of screen space than [facet_grid()] because most
#' displays are roughly rectangular.
#'
#' @param facets A set of variables or expressions quoted by [vars()]
#'   and defining faceting groups on the rows or columns dimension.
#'   The variables can be named (the names are passed to `labeller`).
#'
#'   For compatibility with the classic interface, can also be a
#'   formula or character vector. Use either a one sided formula, `~a + b`,
#'   or a character vector, `c("a", "b")`.
#' @param nrow,ncol Number of rows and columns.
#' @param scales Should scales be fixed (`"fixed"`, the default),
#'   free (`"free"`), or free in one dimension (`"free_x"`,
#'   `"free_y"`)?
#' @param strip.position By default, the labels are displayed on the top of
#'   the plot. Using `strip.position` it is possible to place the labels on
#'   either of the four sides by setting \code{strip.position = c("top",
#'   "bottom", "left", "right")}
#' @param dir Direction: either `"h"` for horizontal, the default, or `"v"`,
#'   for vertical.
#' @param axes Determines which axes will be drawn in case of fixed scales.
#'   When `"margins"` (default), axes will be drawn at the exterior margins.
#'   `"all_x"` and `"all_y"` will draw the respective axes at the interior
#'   panels too, whereas `"all"` will draw all axes at all panels.
#' @param axis.labels Determines whether to draw labels for interior axes when
#'   the scale is fixed and the `axis` argument is not `"margins"`. When
#'   `"all"` (default), all interior axes get labels. When `"margins"`, only
#'   the exterior axes get labels, and the interior axes get none. When
#'   `"all_x"` or `"all_y"`, only draws the labels at the interior axes in the
#'   x- or y-direction respectively.
#' @inheritParams facet_grid
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' # Use vars() to supply faceting variables:
#' p + facet_wrap(vars(class))
#'
#' # Control the number of rows and columns with nrow and ncol
#' p + facet_wrap(vars(class), nrow = 4)
#'
#' \donttest{
#' # You can facet by multiple variables
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(cyl, drv))
#'
#' # Use the `labeller` option to control how labels are printed:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(cyl, drv), labeller = "label_both")
#'
#' # To change the order in which the panels appear, change the levels
#' # of the underlying factor.
#' mpg$class2 <- reorder(mpg$class, mpg$displ)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class2))
#'
#' # By default, the same scales are used for all panels. You can allow
#' # scales to vary across the panels with the `scales` argument.
#' # Free scales make it easier to see patterns within each panel, but
#' # harder to compare across panels.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class), scales = "free")
#'
#' # When scales are constant, duplicated axes can be shown with
#' # or without labels
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class), axes = "all", axis.labels = "all_y")
#'
#' # To repeat the same data in every panel, simply construct a data frame
#' # that does not contain the faceting variable.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(data = transform(mpg, class = NULL), colour = "grey85") +
#'   geom_point() +
#'   facet_wrap(vars(class))
#'
#' # Use `strip.position` to display the facet labels at the side of your
#' # choice. Setting it to `bottom` makes it act as a subtitle for the axis.
#' # This is typically used with free scales and a theme without boxes around
#' # strip labels.
#' ggplot(economics_long, aes(date, value)) +
#'   geom_line() +
#'   facet_wrap(vars(variable), scales = "free_y", nrow = 2, strip.position = "top") +
#'   theme(strip.background = element_blank(), strip.placement = "outside")
#' }
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = deprecated(), drop = TRUE, dir = "h",
                       strip.position = 'top', axes = "margins",
                       axis.labels = "all") {
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # If scales are free, always draw the axes
  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = free$x || any(draw_axes %in% c("all_x", "all")),
    y = free$y || any(draw_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so only omit labels if
  # scales are not free and the axis is to be drawn
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = free$x || !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = free$y || !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  # Flatten all facets dimensions into a single one
  facets <- wrap_as_facets_list(facets)

  if (lifecycle::is_present(switch) && !is.null(switch)) {
    deprecate_warn0("2.2.0", "facet_wrap(switch)", "facet_wrap(strip.position)")
    strip.position <- if (switch == "x") "bottom" else "left"
  }
  strip.position <- arg_match0(strip.position, c("top", "bottom", "left", "right"))

  check_number_whole(ncol, allow_null = TRUE, min = 1)
  check_number_whole(nrow, allow_null = TRUE, min = 1)

  if (identical(dir, "v")) {
    # swap
    tmp <- ncol
    ncol <- nrow
    nrow <- tmp
  }

  ggproto(NULL, FacetWrap,
    shrink = shrink,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      draw_axes = draw_axes,
      axis_labels = axis_labels
    )
  )
}

# Returns a quosures object
wrap_as_facets_list <- function(x) {
  facets_list <- as_facets_list(x)
  compact_facets(facets_list)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetWrap <- ggproto("FacetWrap", Facet,
  shrink = TRUE,

  compute_layout = function(self, data, params) {
    vars <- params$facets
    if (length(vars) == 0) {
      return(layout_null())
    }

    check_facet_vars(names(vars), name = snake_class(self))

    base <- combine_vars(data, params$plot_env, vars, drop = params$drop)

    id <- id(base, drop = TRUE)
    n <- attr(id, "n")

    dims <- wrap_dims(n, params$nrow, params$ncol)
    layout <- data_frame0(
      PANEL = factor(id, levels = seq_len(n)),
      ROW = if (params$as.table) {
        as.integer((id - 1L) %/% dims[2] + 1L)
      } else {
        as.integer(dims[1] - (id - 1L) %/% dims[2])
      },
      COL = as.integer((id - 1L) %% dims[2] + 1L),
      .size = length(id)
    )

    # For vertical direction, flip row and col
    if (identical(params$dir, "v")) {
      layout[c("ROW", "COL")] <- layout[c("COL", "ROW")]
    }

    panels <- vec_cbind(layout, base)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    # Add scale identification
    panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
    panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L

    panels
  },
  map_data = function(data, layout, params) {
    if (empty(data)) {
      return(vec_cbind(data %|W|% NULL, PANEL = integer(0)))
    }

    vars <- params$facets

    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }

    facet_vals <- eval_facets(vars, data, params$.possible_columns)
    facet_vals[] <- lapply(facet_vals[], as.factor)
    layout[] <- lapply(layout[], as.factor)

    missing_facets <- setdiff(names(vars), names(facet_vals))
    if (length(missing_facets) > 0) {

      to_add <- unique0(layout[missing_facets])

      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))

      data <- data[data_rep, , drop = FALSE]
      facet_vals <- vec_cbind(
        facet_vals[data_rep, ,  drop = FALSE],
        to_add[facet_rep, , drop = FALSE]
      )
    }

    keys <- join_keys(facet_vals, layout, by = names(vars))

    data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    data
  },
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      cli::cli_abort("{.fn {snake_class(self)}} can't use free scales with {.fn {snake_class(coord)}}.")
    }

    if (inherits(coord, "CoordFlip")) {
      if (params$free$x) {
        layout$SCALE_X <- seq_len(nrow(layout))
      } else {
        layout$SCALE_X <- 1L
      }
      if (params$free$y) {
        layout$SCALE_Y <- seq_len(nrow(layout))
      } else {
        layout$SCALE_Y <- 1L
      }
    }

    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    n <- nrow(layout)
    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order, ]
    panels <- panels[panel_order]
    panel_pos <- convertInd(layout$ROW, layout$COL, nrow)

    x_axis_order <- if (params$axis_labels$x) layout$SCALE_X else seq(n)
    y_axis_order <- if (params$axis_labels$y) layout$SCALE_Y else seq(n)

    ranges <- censor_labels(ranges, layout, params$axis_labels)
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

    if (length(params$facets) == 0) {
      # Add a dummy label
      labels_df <- data_frame0("(all)" = "(all)", .size = 1)
    } else {
      labels_df <- layout[names(params$facets)]
    }
    attr(labels_df, "facet") <- "wrap"
    strips <- render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme)

    # If user hasn't set aspect ratio, ask the coordinate system if
    # it wants to specify one
    aspect_ratio <- theme$aspect.ratio %||% coord$aspect(ranges[[1]])

    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }

    empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
    panel_table <- empty_table
    panel_table[panel_pos] <- panels
    empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
    panel_table <- gtable_matrix("layout", panel_table,
     widths = unit(rep(1, ncol), "null"),
     heights = unit(rep(abs(aspect_ratio), nrow), "null"), respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

    panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    axis_mat_x_top <- empty_table
    axis_mat_x_top[panel_pos] <- axes$x$top[x_axis_order]
    axis_mat_x_bottom <- empty_table
    axis_mat_x_bottom[panel_pos] <- axes$x$bottom[x_axis_order]
    axis_mat_y_left <- empty_table
    axis_mat_y_left[panel_pos] <- axes$y$left[y_axis_order]
    axis_mat_y_right <- empty_table
    axis_mat_y_right[panel_pos] <- axes$y$right[y_axis_order]
    if (!(params$free$x || params$draw_axes$x)) {
      axis_mat_x_top[-1,]<- list(zeroGrob())
      axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
    }
    if (!(params$free$y || params$draw_axes$y)) {
      axis_mat_y_left[, -1] <- list(zeroGrob())
      axis_mat_y_right[, -ncol] <- list(zeroGrob())
    }


    # Add back missing axes
    if (any(empties)) {
      row_ind <- row(empties)
      col_ind <- col(empties)
      inside <- (theme$strip.placement %||% "inside") == "inside"
      empty_bottom <- apply(empties, 2, function(x) c(diff(x) == 1, FALSE))
      if (any(empty_bottom)) {
        pos <- which(empty_bottom)
        panel_loc <- data_frame0(
          ROW = row_ind[pos],
          COL = col_ind[pos],
          .size = length(pos)
        )
        panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
        x_axes <- axes$x$bottom[x_axis_order[panels]]
        if (params$strip.position == "bottom" &&
            !inside &&
            any(!vapply(x_axes, is.zero, logical(1))) &&
            !params$free$x) {
          cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"bottom\"} and {.code strip.placement == \"outside\"}")
        } else {
          axis_mat_x_bottom[pos] <- x_axes
        }
      }
      empty_top <- apply(empties, 2, function(x) c(FALSE, diff(x) == -1))
      if (any(empty_top)) {
        pos <- which(empty_top)
        panel_loc <- data_frame0(
          ROW = row_ind[pos],
          COL = col_ind[pos],
          .size = length(pos)
        )
        panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
        x_axes <- axes$x$top[x_axis_order[panels]]
        if (params$strip.position == "top" &&
            !inside &&
            any(!vapply(x_axes, is.zero, logical(1))) &&
            !params$free$x) {
          cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"top\"} and {.code strip.placement == \"outside\"}")
        } else {
          axis_mat_x_top[pos] <- x_axes
        }
      }
      empty_right <- t(apply(empties, 1, function(x) c(diff(x) == 1, FALSE)))
      if (any(empty_right)) {
        pos <- which(empty_right)
        panel_loc <- data_frame0(
          ROW = row_ind[pos],
          COL = col_ind[pos],
          .size = length(pos)
        )
        panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
        y_axes <- axes$y$right[y_axis_order[panels]]
        if (params$strip.position == "right" &&
            !inside &&
            any(!vapply(y_axes, is.zero, logical(1))) &&
            !params$free$y) {
          cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"right\"} and {.code strip.placement == \"outside\"}")
        } else {
          axis_mat_y_right[pos] <- y_axes
        }
      }
      empty_left <- t(apply(empties, 1, function(x) c(FALSE, diff(x) == -1)))
      if (any(empty_left)) {
        pos <- which(empty_left)
        panel_loc <- data_frame0(
          ROW = row_ind[pos],
          COL = col_ind[pos],
          .size = length(pos)
        )
        panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
        y_axes <- axes$y$left[y_axis_order[panels]]
        if (params$strip.position == "left" &&
            !inside &&
            any(!vapply(y_axes, is.zero, logical(1))) &&
            !params$free$y) {
          cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"left\"} and {.code strip.placement == \"outside\"}")
        } else {
          axis_mat_y_left[pos] <- y_axes
        }
      }
    }
    panel_table <- weave_axes(
      panel_table,
      axes = list(
        top  = axis_mat_x_top,  bottom = axis_mat_x_bottom,
        left = axis_mat_y_left, right  = axis_mat_y_right
      ),
      empty = empties
    )
    axis_size   <- panel_table$sizes
    panel_table <- panel_table$panels

    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
    if (params$strip.position %in% c("top", "bottom")) {
      inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        placement <- if (inside_x) -1 else -2
        strip_pad <- axis_size$top
      } else {
        placement <- if (inside_x) 0 else 1
        strip_pad <- axis_size$bottom
      }
      strip_height <- unit(apply(strip_mat, 1, max_height, value_only = TRUE), "cm")
      panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
      if (!inside_x) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
      }
    } else {
      inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        placement <- if (inside_y) -1 else -2
        strip_pad <- axis_size$left
      } else {
        placement <- if (inside_y) 0 else 1
        strip_pad <- axis_size$right
      }
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, max_width, value_only = TRUE), "cm")
      panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
      if (!inside_y) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
      }
    }
    panel_table
  },
  vars = function(self) {
    names(self$params$facets)
  }
)


# Helpers -----------------------------------------------------------------

#' Arrange 1d structure into a grid
#'
#' @param n length of structure
#' @param nrow,ncol desired dimensions for the grid
#'
#' @return the grid dimension as a vector with nrow and then ncol
#'
#' @keywords internal
#' @export
wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    rc <- grDevices::n2mfrow(n)
    nrow <- rc[2]
    ncol <- rc[1]
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  if (nrow * ncol < n) {
    cli::cli_abort(c(
      "Need {n} panel{?s}, but together {.arg nrow} and {.arg ncol} only provide {nrow * ncol}.",
      i = "Please increase {.arg ncol} and/or {.arg nrow}."
    ))
  }

  c(nrow, ncol)
}
convertInd <- function(row, col, nrow) {
  (col - 1) * nrow + row
}

weave_tables_col <- function(table, table2, col_shift, col_width, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    table <- gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[, i], t = panel_row, l = col_ind + 1, clip = clip, name = paste0(name, "-", seq_along(panel_row), "-", i), z = z)
    }
  }
  table
}
weave_tables_row <- function(table, table2, row_shift, row_height, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    table <- gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[i, ], t = row_ind + 1, l = panel_col, clip = clip, name = paste0(name, "-", seq_along(panel_col), "-", i), z = z)
    }
  }
  table
}

weave_axes <- function(panels, axes, empty = NULL, z = 3L) {
  empty  <- which(empty %||% matrix(logical(), 0, 0), arr.ind = TRUE)
  sides  <- match(names(axes), .trbl)
  margin <- c(1L, 2L, 1L, 2L)[sides]
  shift  <- c(1L, -1L, -1L, 1L)[sides]
  sizes  <- Map(
    measure_axes, axis = axes, margin = margin, shift = shift,
    MoreArgs = list(empty_idx = empty)
  )
  names <- paste0("axis-", substr(names(axes), 1, 1))
  shift <- c(-1L, 0L, 0L, -1L)[sides]
  weave <- list(weave_tables_row, weave_tables_col)[c(1, 2, 1, 2)][sides]
  for (i in seq_along(axes)) {
    panels <- weave[[i]](panels, axes[[i]], shift[i], sizes[[i]], names[i], z = z)
  }
  list(panels = panels, sizes = sizes)
}

# Measures the size of axes while ignoring those bordering empty panels
measure_axes <- function(empty_idx, axis, margin = 1L, shift = 0) {
  dim  <- dim(axis)

  measure <- switch(margin, height_cm, width_cm)
  cm <- matrix(measure(axis), dim[1], dim[2])

  if (nrow(empty_idx) > 0 && shift != 0) {
    set_zero <- empty_idx
    set_zero[, margin] <- set_zero[, margin] + shift
    keep <- set_zero[, margin] <= dim[margin] & set_zero[, margin] > 0
    set_zero <- set_zero[keep, , drop = FALSE]
  } else {
    set_zero <- matrix(integer(), nrow = 0, ncol = 2)
  }

  cm[set_zero] <- 0
  unit(apply(cm, margin, max), "cm")
}
