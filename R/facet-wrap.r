#' @include facet-.r
NULL

#' Wrap a 1d ribbon of panels into 2d
#'
#' \code{facet_wrap} wraps a 1d sequence of panels into 2d. This is generally
#' a better use of screen space than \code{\link{facet_grid}} because most
#' displays are roughly rectangular.
#'
#' @param facets Either a formula or character vector. Use either a
#'   one sided formula, \code{~a + b}, or a character vector, \code{c("a", "b")}.
#' @param nrow,ncol Number of rows and columns.
#' @param scales should Scales be fixed (\code{"fixed"}, the default),
#'   free (\code{"free"}), or free in one dimension (\code{"free_x"},
#'   \code{"free_y"}).
#' @param strip.position By default, the labels are displayed on the top of
#'   the plot. Using \code{strip.position} it is possible to place the labels on
#'   either of the four sides by setting \code{strip.position = c("top",
#'   "bottom", "left", "right")}
#' @param dir Direction: either "h" for horizontal, the default, or "v", for
#'   vertical.
#' @inheritParams facet_grid
#' @export
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~class)
#'
#' # Control the number of rows and columns with nrow and ncol
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~class, nrow = 4)
#'
#' \donttest{
#' # You can facet by multiple variables
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~ cyl + drv)
#' # Or use a character vector:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(c("cyl", "drv"))
#'
#' # Use the `labeller` option to control how labels are printed:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(c("cyl", "drv"), labeller = "label_both")
#'
#' # To change the order in which the panels appear, change the levels
#' # of the underlying factor.
#' mpg$class2 <- reorder(mpg$class, mpg$displ)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~class2)
#'
#' # By default, the same scales are used for all panels. You can allow
#' # scales to vary across the panels with the `scales` argument.
#' # Free scales make it easier to see patterns within each panel, but
#' # harder to compare across panels.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~class, scales = "free")
#'
#' # To repeat the same data in every panel, simply construct a data frame
#' # that does not contain the facetting variable.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(data = transform(mpg, class = NULL), colour = "grey85") +
#'   geom_point() +
#'   facet_wrap(~class)
#'
#' # Use `strip.position` to display the facet labels at the side of your
#' # choice. Setting it to `bottom` makes it act as a subtitle for the axis.
#' # This is typically used with free scales and a theme without boxes around
#' # strip labels.
#' ggplot(economics_long, aes(date, value)) +
#'   geom_line() +
#'   facet_wrap(~variable, scales = "free_y", nrow = 2, strip.position = "bottom") +
#'   theme(strip.background = element_blank(), strip.placement = "outside")
#' }
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = NULL, drop = TRUE, dir = "h", strip.position = 'top') {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  if (!is.null(switch)) {
    .Deprecated("strip.position", old = "switch")
    strip.position <- if (switch == "x") "bottom" else "left"
  }
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right"))
  if (identical(dir, "v")) {
    # swap
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- sanitise_dim(nrow_swap)
    ncol <- sanitise_dim(ncol_swap)
  } else {
    nrow <- sanitise_dim(nrow)
    ncol <- sanitise_dim(ncol)
  }

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  ggproto(NULL, FacetWrap,
    shrink = shrink,
    params = list(facets = as.quoted(facets), free = free,
    as.table = as.table, strip.position = strip.position,
    drop = drop, ncol = ncol, nrow = nrow,
    labeller = labeller,
    dir = dir)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetWrap <- ggproto("FacetWrap", Facet,
  shrink = TRUE,

  compute_layout = function(data, params) {
    vars <- as.quoted(params$facets)
    if (length(vars) == 0) return(layout_null())

    base <- plyr::unrowname(
      combine_vars(data, params$plot_env, vars, drop = params$drop)
    )

    id <- plyr::id(base, drop = TRUE)
    n <- attr(id, "n")

    dims <- wrap_dims(n, params$nrow, params$ncol)
    layout <- data.frame(PANEL = factor(id, levels = seq_len(n)))

    if (params$as.table) {
      layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
    } else {
      layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2])
    }
    layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)

    # For vertical direction, flip row and col
    if (identical(params$dir, "v")) {
      layout[c("ROW", "COL")] <- layout[c("COL", "ROW")]
    }

    panels <- cbind(layout, plyr::unrowname(base))
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    # Add scale identification
    panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
    panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L

    panels
  },
  map_data = function(data, layout, params) {
    if (empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    vars <- as.quoted(params$facets)

    facet_vals <- eval_facet_vars(vars, data, params$plot_env)
    facet_vals[] <- lapply(facet_vals[], as.factor)

    missing_facets <- setdiff(names(vars), names(facet_vals))
    if (length(missing_facets) > 0) {

      to_add <- unique(layout[missing_facets])

      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))

      data <- plyr::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- plyr::unrowname(cbind(
        facet_vals[data_rep, ,  drop = FALSE],
        to_add[facet_rep, , drop = FALSE]))
    }

    keys <- plyr::join.keys(facet_vals, layout, by = names(vars))

    data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    data[order(data$PANEL), ]
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    # If coord is non-cartesian and (x is free or y is free)
    # then throw error
    if ((!inherits(coord, "CoordCartesian")) && (params$free$x || params$free$y)) {
      stop("ggplot2 does not currently support free scales with a non-cartesian coord", call. = FALSE)
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

    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

    labels_df <- layout[names(params$facets)]
    attr(labels_df, "facet") <- "wrap"
    strips <- render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme)

    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }

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
     heights = unit(rep(aspect_ratio, nrow), "null"), respect = respect, clip = "on", z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

    panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    axis_mat_x_top <- empty_table
    axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
    axis_mat_x_bottom <- empty_table
    axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
    axis_mat_y_left <- empty_table
    axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
    axis_mat_y_right <- empty_table
    axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]
    if (!params$free$x) {
      axis_mat_x_top[-1,]<- list(zeroGrob())
      axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
    }
    if (!params$free$y) {
      axis_mat_y_left[, -1] <- list(zeroGrob())
      axis_mat_y_right[, -ncol] <- list(zeroGrob())
    }
    axis_height_top <- unit(apply(axis_mat_x_top, 1, max_height), "cm")
    axis_height_bottom <- unit(apply(axis_mat_x_bottom, 1, max_height), "cm")
    axis_width_left <- unit(apply(axis_mat_y_left, 2, max_width), "cm")
    axis_width_right <- unit(apply(axis_mat_y_right, 2, max_width), "cm")
    # Add back missing axes
    if (any(empties)) {
      first_row <- which(apply(empties, 1, any))[1] - 1
      first_col <- which(apply(empties, 2, any))[1] - 1
      row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
      row_pos <- convertInd(layout$ROW[row_panels], layout$COL[row_panels], nrow)
      row_axes <- axes$x$bottom[layout$SCALE_X[row_panels]]
      col_panels <- which(layout$ROW > first_row & layout$COL == first_col)
      col_pos <- convertInd(layout$ROW[col_panels], layout$COL[col_panels], nrow)
      col_axes <- axes$y$right[layout$SCALE_Y[col_panels]]
      if (params$strip.position == "bottom" &&
          theme$strip.placement != "inside" &&
          any(!vapply(row_axes, is.zero, logical(length(row_axes))))) {
        warning("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'", call. = FALSE)
      } else {
        axis_mat_x_bottom[row_pos] <- row_axes
      }
      if (params$strip.position == "right" &&
          theme$strip.placement != "inside" &&
          any(!vapply(col_axes, is.zero, logical(length(col_axes))))) {
        warning("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'", call. = FALSE)
      } else {
        axis_mat_y_right[col_pos] <- col_axes
      }
    }
    panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
    panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
    if (params$strip.position %in% c("top", "bottom")) {
      inside <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        placement <- if (inside) -1 else -2
        strip_pad <- axis_height_top
      } else {
        placement <- if (inside) 0 else 1
        strip_pad <- axis_height_bottom
      }
      strip_height <- unit(apply(strip_mat, 1, max_height), "cm")
      panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, "on")
      if (!inside) {
        strip_pad[unclass(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
      }
    } else {
      inside <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        placement <- if (inside) -1 else -2
        strip_pad <- axis_width_left
      } else {
        placement <- if (inside) 0 else 1
        strip_pad <- axis_width_right
      }
      strip_pad[unclass(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, max_width), "cm")
      panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, "on")
      if (!inside) {
        strip_pad[unclass(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
      }
    }
    panel_table
  }
)


# Helpers -----------------------------------------------------------------

#' Sanitise the number of rows or columns
#'
#' Cleans up the input to be an integer greater than or equal to one, or
#' \code{NULL}. Intended to be used on the \code{nrow} and \code{ncol}
#' arguments of \code{facet_wrap}.
#' @param n Hopefully an integer greater than or equal to one, or \code{NULL},
#' though other inputs are handled.
#' @return An integer greater than or equal to one, or \code{NULL}.
#' @note If the length of the input is greater than one, only the first element
#' is returned, with a warning.
#' If the input is not an integer, it will be coerced to be one.
#' If the value is less than one, \code{NULL} is returned, effectively ignoring
#' the argument.
#' Multiple warnings may be generated.
#' @examples
#' # Valid input just gets returns unchanged
#' sanitise_dim(1)
#' sanitise_dim(NULL)
#'
#' # Only the first element of vectors get returned
#' sanitise_dim(10:1)
#' # Non-integer values are coerced to integer
#' sanitise_dim(pi)
#' # Missing values, values less than one and non-numeric values are
#' # treated as NULL
#' sanitise_dim(NA_integer_)
#' sanitise_dim(0)
#' sanitise_dim("foo")
#' @noRd
sanitise_dim <- function(n) {
  xname <- paste0("`", deparse(substitute(n)), "`")
  if (length(n) == 0) {
    if (!is.null(n)) {
      warning(xname, " has length zero and will be treated as NULL.",
        call. = FALSE)
    }
    return(NULL)
  }
  if (length(n) > 1) {
    warning("Only the first value of ", xname, " will be used.", call. = FALSE)
    n <- n[1]
  }
  if (!is.numeric(n) || (!is.na(n) && n != round(n))) {
    warning("Coercing ", xname, " to be an integer.", call. = FALSE)
    n <- as.integer(n)
  }
  if (is.na(n) || n < 1) {
    warning(xname, " is missing or less than 1 and will be treated as NULL.",
      call. = FALSE)
    return(NULL)
  }
  n
}

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
  stopifnot(nrow * ncol >= n)

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
