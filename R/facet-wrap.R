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
#' @param space If `"fixed"` (default), all panels have the same size and
#'   the number of rows and columns in the layout can be arbitrary. If
#'   `"free_x"`, panels have widths proportional to the length of the x-scale,
#'   but the layout is constrained to one row. If `"free_y"`, panels have
#'   heights proportional to the length of the y-scale, but the layout is
#'   constrained to one column.
#' @param strip.position By default, the labels are displayed on the top of
#'   the plot. Using `strip.position` it is possible to place the labels on
#'   either of the four sides by setting \code{strip.position = c("top",
#'   "bottom", "left", "right")}
#' @param dir Direction: either `"h"` for horizontal, the default, or `"v"`,
#'   for vertical. When `"h"` or `"v"` will be combined with `as.table` to
#'   set final layout. Alternatively, a combination of `"t"` (top) or
#'   `"b"` (bottom) with `"l"` (left) or `"r"` (right) to set a layout directly.
#'   These two letters give the starting position and the first letter gives
#'   the growing direction. For example `"rt"` will place the first panel in
#'   the top-right and starts filling in panels right-to-left.
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
#' @seealso
#' The `r link_book("facet wrap section", "facet#sec-facet-wrap")`
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
#'
#' # The two letters determine the starting position, so 'tr' starts
#' # in the top-right.
#' # The first letter determines direction, so 'tr' fills top-to-bottom.
#' # `dir = "tr"` is equivalent to `dir = "v", as.table = FALSE`
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class), dir = "tr")
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       space = "fixed", shrink = TRUE, labeller = "label_value",
                       as.table = TRUE, switch = deprecated(), drop = TRUE,
                       dir = "h", strip.position = 'top', axes = "margins",
                       axis.labels = "all") {
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v", "lt", "tl", "lb", "bl", "rt", "tr", "rb", "br"))
  if (nchar(dir) == 1) {
    dir <- base::switch(
      dir,
      h = if (as.table) "lt" else "lb",
      v = if (as.table) "tl" else "tr"
    )
  }

  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # We cannot have free space in both directions
  space <- arg_match0(space, c("free_x", "free_y", "fixed"))
  space_free <- list(x = space == "free_x", y = space == "free_y")
  if (space_free$x) {
    if ((nrow %||% 1) != 1 || !is.null(ncol)) {
      cli::cli_warn(
        "Cannot use {.code space = \"free_x\"} with custom \\
        {.arg nrow} or {.arg ncol}."
      )
    }
    ncol <- NULL
    nrow <- 1L
  }
  if (space_free$y) {
    if ((ncol %||% 1) != 1 || !is.null(nrow)) {
      cli::cli_warn(
        "Cannot use {.code space= \"free_y\"} with custom \\
        {.arg nrow} or {.arg ncol}."
      )
    }
    ncol <- 1L
    nrow <- NULL
  }

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
  facets <- compact_facets(facets)

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
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      space_free = space_free,
      labeller = labeller,
      dir = dir,
      draw_axes = draw_axes,
      axis_labels = axis_labels
    )
  )
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
    layout <- wrap_layout(id, dims, params$dir)

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
    facet_vals[] <- lapply(facet_vals[], as_unordered_factor)
    layout[] <- lapply(layout[], as_unordered_factor)

    missing_facets <- setdiff(names(vars), names(facet_vals))
    if (length(missing_facets) > 0) {

      to_add <- unique0(layout[missing_facets])

      data_rep <- rep.int(seq_len(nrow(data)), nrow(to_add))
      facet_rep <- rep(seq_len(nrow(to_add)), each = nrow(data))

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

  attach_axes = function(table, layout, ranges, coord, theme, params) {

    # Setup parameters
    draw_axes   <- params$draw_axes   %||% list(x = FALSE, y = FALSE)
    axis_labels <- params$axis_labels %||% list(x = TRUE,  y = TRUE)
    free        <- params$free        %||% list(x = FALSE, y = FALSE)

    # Render individual axes
    ranges   <- censor_labels(ranges, layout, axis_labels)
    original <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

    # Sort axes
    x_order <- if (axis_labels$x) layout$SCALE_X else seq_len(nrow(layout))
    y_order <- if (axis_labels$y) layout$SCALE_Y else seq_len(nrow(layout))
    original$x <- lapply(original$x, `[`, i = x_order)
    original$y <- lapply(original$y, `[`, i = y_order)

    # Setup matrices for axes
    dim <- c(max(layout$ROW), max(layout$COL))
    index <- convertInd(layout$ROW, layout$COL, dim[1])
    empty <- matrix(list(zeroGrob()), dim[1], dim[2])
    top <- bottom <- left <- right <- empty

    # Fill axis matrices
    top[index]    <- original$x$top
    bottom[index] <- original$x$bottom
    left[index]   <- original$y$left
    right[index]  <- original$y$right

    # Suppress interior axes
    if (!(free$x || draw_axes$x)) {
      top[-1, ]         <- list(zeroGrob())
      bottom[-dim[1], ] <- list(zeroGrob())
    }
    if (!(free$y || draw_axes$y)) {
      left[, -1]       <- list(zeroGrob())
      right[, -dim[2]] <- list(zeroGrob())
    }

    # Check for empty panels and exit early if there are none
    empty <- matrix(TRUE, dim[1], dim[2])
    empty[index] <- FALSE
    if (!any(empty)) {
      axes <- list(top = top, bottom = bottom, left = left, right = right)
      return(weave_axes(table, axes, empty))
    }

    # Match empty table to layout
    matched <- vec_match(
      data_frame0(ROW = as.vector(row(empty)), COL = as.vector(col(empty))),
      layout[, c("ROW", "COL")]
    )

    # Figure out where axes should be added back
    empty_bottom <- which(  apply(empty, 2, function(x) c(diff(x) == 1, FALSE)))
    empty_top    <- which(  apply(empty, 2, function(x) c(FALSE, diff(x) == -1)))
    empty_right  <- which(t(apply(empty, 1, function(x) c(diff(x) == 1, FALSE))))
    empty_left   <- which(t(apply(empty, 1, function(x) c(FALSE, diff(x) == -1))))

    # Keep track of potential clashes between strips and axes
    inside <- (theme$strip.placement %||% "inside") == "inside"
    strip  <- params$strip.position %||% "top"
    clash  <- c(top = FALSE, bottom = FALSE, left = FALSE, right = FALSE)

    # Go through every position and place back axes
    if (length(empty_bottom) > 0) {
      x_axes <- original$x$bottom[matched[empty_bottom]]
      clash["bottom"] <- strip == "bottom" && !inside && !free$x &&
        !all(vapply(x_axes, is.zero, logical(1)))
      if (!clash["bottom"]) {
        bottom[empty_bottom] <- x_axes
      }
    }

    if (length(empty_top) > 0) {
      x_axes <- original$x$top[matched[empty_top]]
      clash["top"] <- strip == "top" && !inside && !free$x &&
        !all(vapply(x_axes, is.zero, logical(1)))
      if (!clash["top"]) {
        top[empty_top] <- x_axes
      }
    }

    if (length(empty_right) > 0) {
      y_axes <- original$y$right[matched[empty_right]]
      clash["right"]  <- strip == "right" && !inside && !free$y &&
        !all(vapply(y_axes, is.zero, logical(1)))
      if (!clash["right"]) {
        right[empty_right] <- y_axes
      }
    }

    if (length(empty_left) > 0) {
      y_axes <- original$y$left[matched[empty_left]]
      clash["left"]  <- strip == "left" && !inside && !free$y &&
        !all(vapply(y_axes, is.zero, logical(1)))
      if (!clash["left"]) {
        left[empty_left] <- y_axes
      }
    }

    if (any(clash)) {
      cli::cli_warn(
        "Suppressing axis rendering when \\
        {.code strip.position =\"{strip}\"} and \\
        {.code strip.placement = \"outside\".}"
      )
    }

    axes <- list(top = top, bottom = bottom, left = left, right = right)
    weave_axes(table, axes, empty)
  },

  attach_strips = function(self, table, layout, params, theme) {

    # Format labels
    strips <- self$format_strip_labels(layout, params)
    strips <- render_strips(strips$facets, strips$facets, theme = theme)

    # Set position invariant parameters
    padding  <- convertUnit(calc_element("strip.switch.pad.wrap", theme), "cm")
    position <- params$strip.position %||% "top"
    pos      <- substr(position, 1, 1)
    prefix   <- paste0("strip-", pos)

    # Setup weaving table
    dim <- c(max(layout$ROW), max(layout$COL))
    index <- convertInd(layout$ROW, layout$COL, dim[1])
    mat <- matrix(list(zeroGrob()), dim[1], dim[2])
    mat[index] <- unlist(unname(strips), recursive = FALSE)[[position]]

    # Setup orientation dependent parameters
    if (position %in% c("top", "bottom")) {
      inside  <- "strip.placement.x"
      size    <- apply(mat, 1, max_height, value_only = TRUE)
      weave   <- weave_tables_row
    } else {
      inside  <- "strip.placement.y"
      size    <- apply(mat, 2, max_width, value_only = TRUE)
      weave   <- weave_tables_col
    }

    inside <- (calc_element(inside, theme) %||% "inside") == "inside"
    shift  <- switch(position, top = , left = c(-1, -2), c(0, 1))
    shift  <- if (inside) shift[1] else shift[2]
    size   <- unit(size, "cm")

    table <- weave(table, mat, shift, size, name = prefix, z = 2, clip = "off")

    if (!inside) {
      axes  <- grepl(paste0("axis-", pos), table$layout$name)
      has_axes <- !vapply(table$grobs[axes], is.zero, logical(1))
      has_axes <- split(has_axes, table$layout[[pos]][axes])
      has_axes <- vapply(has_axes, sum, numeric(1)) > 0
      padding  <- rep(padding, length(has_axes))
      padding[!has_axes] <- unit(0, "cm")
      table <- weave(table, , shift, padding)
    }

    table
  },

  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
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

    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order, ]
    panels <- panels[panel_order]

    ggproto_parent(Facet, self)$draw_panels(
      panels = panels, layout = layout,
      ranges = ranges, coord = coord,
      theme = theme, params = params
    )
  },
  vars = function(self) {
    names(self$params$facets)
  },

  format_strip_labels = function(layout, params) {
    if (length(params$facets) == 0) {
      labels <- data_frame0("(all)" = "(all)", .size = 1)
    } else {
      labels <- layout[intersect(names(params$facets), names(layout))]
    }
    if (empty(labels)) {
      return(NULL)
    }
    attr(labels, "facet") <- "wrap"
    attr(labels, "type") <- switch(params$strip.position, left = , right = "rows", "cols")

    labeller <- match.fun(params$labeller)
    list(facets = data_frame0(!!!labeller(labels)))
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
  panels
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

wrap_layout <- function(id, dims, dir) {
  as.table <- TRUE
  n <- attr(id, "n")

  if (nchar(dir) != 2) {
    # Should only occur when `as.table` was not incorporated into `dir`
    dir <- switch(dir, h = "lt", v = "tl")
    deprecate_soft0(
      "3.5.2",
      what = I("Internal use of `dir = \"h\"` and `dir = \"v\"` in `facet_wrap()`"),
      details = I(c(
        "The `dir` argument should incorporate the `as.table` argument.",
        paste0("Falling back to `dir = \"", dir, "\"`.")
      ))
    )
  }

  dir <- arg_match0(dir, c("lt", "tl", "lb", "bl", "rt", "tr", "rb", "br"))

  ROW <- switch(
    dir,
    lt = , rt = (id - 1L) %/% dims[2] + 1L,
    tl = , tr = (id - 1L) %%  dims[1] + 1L,
    lb = , rb = dims[1] - (id - 1L) %/% dims[2],
    bl = , br = dims[1] - (id - 1L) %%  dims[1]
  )

  COL <- switch(
    dir,
    lt = , lb = (id - 1L) %% dims[2] + 1L,
    tl = , bl = (id - 1L) %/% dims[1] + 1L,
    rt = , rb = dims[2] - (id - 1L) %%  dims[2],
    tr = , br = dims[2] - (id - 1L) %/% dims[1]
  )

  data_frame0(
    PANEL = factor(id, levels = seq_len(n)),
    ROW   = as.integer(ROW),
    COL   = as.integer(COL),
    .size = length(id)
  )
}
