#' @include facet-.R
NULL

#' Lay out panels in a grid
#'
#' `facet_grid()` forms a matrix of panels defined by row and column
#' faceting variables. It is most useful when you have two discrete
#' variables, and all combinations of the variables exist in the data.
#' If you have only one variable with many levels, try [facet_wrap()].
#'
#' @param rows,cols A set of variables or expressions quoted by
#'   [vars()] and defining faceting groups on the rows or columns
#'   dimension. The variables can be named (the names are passed to
#'   `labeller`).
#'
#'   For compatibility with the classic interface, `rows` can also be
#'   a formula with the rows (of the tabular display) on the LHS and
#'   the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this
#'   dimension (either row or column).
#' @param scales Are scales shared across all facets (the default,
#'   `"fixed"`), or do they vary across rows (`"free_x"`),
#'   columns (`"free_y"`), or both rows and columns (`"free"`)?
#' @param space If `"fixed"`, the default, all panels have the same size.
#'   If `"free_y"` their height will be proportional to the length of the
#'   y scale; if `"free_x"` their width will be proportional to the
#'  length of the x scale; or if `"free"` both height and width will
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes one data frame of labels and
#'   returns a list or data frame of character vectors. Each input
#'   column corresponds to one factor. Thus there will be more than
#'   one with `vars(cyl, am)`. Each output
#'   column gets displayed as one separate line in the strip
#'   label. This function should inherit from the "labeller" S3 class
#'   for compatibility with [labeller()]. You can use different labeling
#'   functions for different kind of labels, for example use [label_parsed()] for
#'   formatting facet labels. [label_value()] is used by default,
#'   check it for more details and pointers to other options.
#' @param as.table If `TRUE`, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If `FALSE`, the
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param switch By default, the labels are displayed on the top and
#'   right of the plot. If `"x"`, the top labels will be
#'   displayed to the bottom. If `"y"`, the right-hand side
#'   labels will be displayed to the left. Can also be set to
#'   `"both"`.
#' @param shrink If `TRUE`, will shrink scales to fit output of
#'   statistics, not raw data. If `FALSE`, will be range of raw data
#'   before statistical summary.
#' @param drop If `TRUE`, the default, all factor levels not used in the
#'   data will automatically be dropped. If `FALSE`, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @param margins Either a logical value or a character
#'   vector. Margins are additional facets which contain all the data
#'   for each of the possible values of the faceting variables. If
#'   `FALSE`, no additional facets are included (the
#'   default). If `TRUE`, margins are included for all faceting
#'   variables. If specified as a character vector, it is the names of
#'   variables for which margins are to be created.
#' @param facets `r lifecycle::badge("deprecated")` Please use `rows`
#'   and `cols` instead.
#' @param axes Determines which axes will be drawn. When `"margins"`
#'   (default), axes will be drawn at the exterior margins. `"all_x"` and
#'   `"all_y"` will draw the respective axes at the interior panels too, whereas
#'   `"all"` will draw all axes at all panels.
#' @param axis.labels Determines whether to draw labels for interior axes when
#'   the `axes` argument is not `"margins"`. When `"all"` (default), all
#'   interior axes get labels. When `"margins"`, only the exterior axes get
#'   labels and the interior axes get none. When `"all_x"` or `"all_y"`, only
#'   draws the labels at the interior axes in the x- or y-direction
#'   respectively.
#' @export
#' @seealso
#' The `r link_book("facet grid section", "facet#facet-grid")`
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point()
#'
#' # Use vars() to supply variables from the dataset:
#' p + facet_grid(rows = vars(drv))
#' p + facet_grid(cols = vars(cyl))
#' p + facet_grid(vars(drv), vars(cyl))
#'
#' # To change plot order of facet grid,
#' # change the order of variable levels with factor()
#'
#' # If you combine a facetted dataset with a dataset that lacks those
#' # faceting variables, the data will be repeated across the missing
#' # combinations:
#' df <- data.frame(displ = mean(mpg$displ), cty = mean(mpg$cty))
#' p +
#'   facet_grid(cols = vars(cyl)) +
#'   geom_point(data = df, colour = "red", size = 2)
#'
#' # When scales are constant, duplicated axes can be shown with
#' # or without labels
#' ggplot(mpg, aes(cty, hwy)) +
#'   geom_point() +
#'   facet_grid(year ~ drv, axes = "all", axis.labels = "all_x")
#'
#' # Free scales -------------------------------------------------------
#' # You can also choose whether the scales should be constant
#' # across all panels (the default), or whether they should be allowed
#' # to vary
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point()
#'
#' mt + facet_grid(vars(cyl), scales = "free")
#'
#' # If scales and space are free, then the mapping between position
#' # and values in the data will be the same across all panels. This
#' # is particularly useful for categorical axes
#' ggplot(mpg, aes(drv, model)) +
#'   geom_point() +
#'   facet_grid(manufacturer ~ ., scales = "free", space = "free") +
#'   theme(strip.text.y = element_text(angle = 0))
#'
#' # Margins ----------------------------------------------------------
#' \donttest{
#' # Margins can be specified logically (all yes or all no) or for specific
#' # variables as (character) variable names
#' mg <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' mg + facet_grid(vs + am ~ gear, margins = TRUE)
#' mg + facet_grid(vs + am ~ gear, margins = "am")
#' # when margins are made over "vs", since the facets for "am" vary
#' # within the values of "vs", the marginal facet for "vs" is also
#' # a margin over "am".
#' mg + facet_grid(vs + am ~ gear, margins = "vs")
#' }
facet_grid <- function(rows = NULL, cols = NULL, scales = "fixed",
                       space = "fixed", shrink = TRUE,
                       labeller = "label_value", as.table = TRUE,
                       switch = NULL, drop = TRUE, margins = FALSE,
                       axes = "margins", axis.labels = "all",
                       facets = deprecated()) {
  # `facets` is deprecated and renamed to `rows`
  if (lifecycle::is_present(facets)) {
    deprecate_warn0("2.2.0", "facet_grid(facets)", "facet_grid(rows)")
    rows <- facets
  }

  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }

  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  space <- arg_match0(space %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )

  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = any(draw_axes %in% c("all_x", "all")),
    y = any(draw_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so even when no internal axes
  # are to be drawn, register as labelled.
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )

  if (!is.null(switch)) {
    arg_match0(switch, c("both", "x", "y"))
  }

  facets_list <- grid_as_facets_list(rows, cols)

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  ggproto(NULL, FacetGrid,
    shrink = shrink,
    params = list(rows = facets_list$rows, cols = facets_list$cols, margins = margins,
      free = free, space_free = space_free, labeller = labeller,
      as.table = as.table, switch = switch, drop = drop,
      draw_axes = draw_axes, axis_labels = axis_labels)
  )
}

# Returns a list of quosures objects. The list has exactly two elements, `rows` and `cols`.
grid_as_facets_list <- function(rows, cols) {
  is_rows_vars <- is.null(rows) || is_quosures(rows)
  if (!is_rows_vars) {
    if (!is.null(cols)) {
      msg <- "{.arg rows} must be {.code NULL} or a {.fn vars} list if {.arg cols} is a {.fn vars} list."
      # Native pipe have higher precedence than + so any type of gg object can be
      # expected here, not just ggplot
      if (inherits(rows, "gg")) {
        msg <- c(
          msg,
          "i" = "Did you use {.code %>%} or {.code |>} instead of {.code +}?"
        )
      }
      cli::cli_abort(msg)
    }
    # For backward-compatibility
    facets_list <- as_facets_list(rows)
    if (length(facets_list) > 2L) {
      cli::cli_abort("A grid facet specification can't have more than two dimensions.")
    }
    # Fill with empty quosures
    facets <- list(rows = quos(), cols = quos())
    facets[seq_along(facets_list)] <- facets_list
    # Do not compact the legacy specs
    return(facets)
  }

  check_object(cols, is_quosures, "a {.fn vars} specification", allow_null = TRUE)

  list(
    rows = compact_facets(as_facets_list(rows)),
    cols = compact_facets(as_facets_list(cols))
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetGrid <- ggproto("FacetGrid", Facet,
  shrink = TRUE,

  compute_layout = function(self, data, params) {
    rows <- params$rows
    cols <- params$cols

    check_facet_vars(names(rows), names(cols), name = snake_class(self))

    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      cli::cli_abort(c(
              "Faceting variables can only appear in {.arg rows} or {.arg cols}, not both.",
        "i" = "Duplicated variables: {.val {dups}}"
      ), call = call2(snake_class(self)))
    }

    base_rows <- combine_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
      base_rows[] <- lapply(base_rows, rev_order)
    }
    base_cols <- combine_vars(data, params$plot_env, cols, drop = params$drop)
    base <- df.grid(base_rows, base_cols)

    if (nrow(base) == 0) {
      return(data_frame0(
        PANEL = factor(1L),
        ROW = 1L,
        COL = 1L,
        SCALE_X = 1L,
        SCALE_Y = 1L
      ))
    }

    # Add margins
    base <- reshape_add_margins(base, list(names(rows), names(cols)), params$margins)
    base <- unique0(base)

    # Create panel info dataset
    panel <- id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))

    rows <- if (!length(names(rows))) rep(1L, length(panel)) else id(base[names(rows)], drop = TRUE)
    cols <- if (!length(names(cols))) rep(1L, length(panel)) else id(base[names(cols)], drop = TRUE)

    panels <- data_frame0(PANEL = panel, ROW = rows, COL = cols, base)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    panels$SCALE_X <- if (params$free$x) panels$COL else 1L
    panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

    panels
  },
  map_data = function(data, layout, params) {
    if (empty(data)) {
      return(vec_cbind(data %|W|% NULL, PANEL = integer(0)))
    }

    rows <- params$rows
    cols <- params$cols
    vars <- c(names(rows), names(cols))

    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }

    # Compute faceting values and add margins
    margin_vars <- list(intersect(names(rows), names(data)),
      intersect(names(cols), names(data)))
    data <- reshape_add_margins(data, margin_vars, params$margins)

    facet_vals <- eval_facets(c(rows, cols), data, params$.possible_columns)

    # If any faceting variables are missing, add them in by
    # duplicating the data
    missing_facets <- setdiff(vars, names(facet_vals))
    if (length(missing_facets) > 0) {
      to_add <- unique0(layout[missing_facets])

      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))

      data <- unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- unrowname(vec_cbind(
        unrowname(facet_vals[data_rep, ,  drop = FALSE]),
        unrowname(to_add[facet_rep, , drop = FALSE]))
      )
    }

    # Add PANEL variable
    if (nrow(facet_vals) == 0) {
      # Special case of no faceting
      data$PANEL <- NO_PANEL
    } else {
      facet_vals[] <- lapply(facet_vals[], as_unordered_factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      layout[] <- lapply(layout[], as_unordered_factor)

      keys <- join_keys(facet_vals, layout, by = vars)

      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },

  attach_axes = function(table, layout, ranges, coord, theme, params) {

    # Setup parameters
    draw_axes   <- params$draw_axes   %||% list(x = FALSE, y = FALSE)
    axis_labels <- params$axis_labels %||% list(x = TRUE,  y = TRUE)

    dim <- c(max(layout$ROW), max(layout$COL))
    if (!axis_labels$x) {
      cols    <- seq_len(nrow(layout))
      x_order <- as.integer(layout$PANEL[order(layout$ROW, layout$COL)])
    } else {
      cols    <- which(layout$ROW == 1)
      x_order <- layout$COL
    }
    if (!axis_labels$y) {
      rows    <- seq_len(nrow(layout))
      y_order <- as.integer(layout$PANEL[order(layout$ROW, layout$COL)])
    } else {
      rows    <- which(layout$COL == 1)
      y_order <- layout$ROW
    }

    # Render individual axes
    ranges <- censor_labels(ranges, layout, axis_labels)
    axes   <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)
    mtx    <- function(x, o) matrix(x[o], dim[1], dim[2], byrow = TRUE)

    if (draw_axes$x) {
      table <- weave_axes(table, lapply(axes$x, mtx, o = x_order))
    } else {
      table <- seam_table(table, axes$x$top,    side = "top",    name = "axis-t", z = 3)
      table <- seam_table(table, axes$x$bottom, side = "bottom", name = "axis-b", z = 3)
    }

    if (draw_axes$y) {
      table <- weave_axes(table, lapply(axes$y, mtx, o = y_order))
    } else {
      table <- seam_table(table, axes$y$left,  side = "left",  name = "axis-l", z = 3)
      table <- seam_table(table, axes$y$right, side = "right", name = "axis-r", z = 3)
    }

    table
  },

  attach_strips = function(table, layout, params, theme) {

    col_vars <- unique0(layout[names(params$cols)])
    row_vars <- unique0(layout[names(params$rows)])
    attr(col_vars, "type")  <- "cols"
    attr(row_vars, "type")  <- "rows"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "facet") <- "grid"

    strips  <- render_strips(col_vars, row_vars, params$labeller, theme)
    padding <- convertUnit(calc_element("strip.switch.pad.grid", theme), "cm")

    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    inside_x <- (calc_element("strip.placement.x", theme) %||% "inside") == "inside"
    shift_x  <- if (inside_x) 1 else 2

    if (switch_x) {
      space <- if (!inside_x & table_has_grob(table, "axis-b")) padding
      table <- seam_table(
        table, strips$x$bottom, side = "bottom", name = "strip-b",
        shift = shift_x, z = 2, clip = "on", spacing = space
      )
    } else {
      space <- if (!inside_x & table_has_grob(table, "axis-t")) padding
      table <- seam_table(
        table, strips$x$top, side = "top", name = "strip-t",
        shift = shift_x, z = 2, clip = "on", spacing = space
      )
    }

    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    inside_y <- (calc_element("strip.placement.y", theme) %||% "inside") == "inside"
    shift_y  <- if (inside_y) 1 else 2

    if (switch_y) {
      space <- if (!inside_y & table_has_grob(table, "axis-l")) padding
      table <- seam_table(
        table, strips$y$left, side = "left", name = "strip-l",
        shift = shift_y, z = 2, clip = "on", spacing = space
      )
    } else {
      space <- if (!inside_y & table_has_grob(table, "axis-r")) padding
      table <- seam_table(
        table, strips$y$right, side = "right", name = "strip-r",
        shift = shift_y, z = 2, clip = "on", spacing = space
      )
    }
    table
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  }
)

# Helpers -----------------------------------------------------------------

ulevels <- function(x, na.last = TRUE) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique0(x), na.last = na.last)
  }
}

table_has_grob <- function(table, pattern) {
  grobs <- table$grobs[grep(pattern, table$layout$name)]
  !all(vapply(grobs, is.zero, logical(1)))
}

seam_table <- function(table, grobs = NULL, side, shift = 1, name, z = 1,
                       clip = "off", spacing = NULL) {
  if (is.null(grobs)) {
    return(table)
  }

  panel_col <- panel_cols(table)
  panel_row <- panel_rows(table)

  row <- switch(
    side,
    bottom = max(panel_row$b) + shift - 1L,
    top    = min(panel_row$t) - shift,
    panel_row$t
  )

  col <- switch(
    side,
    right = max(panel_col$r) + shift - 1L,
    left  = min(panel_col$l) - shift,
    panel_col$l
  )

  if (!is.null(spacing)) {
    table <- switch(
      side,
      bottom = , top = gtable_add_rows(table, spacing, row),
      left = , right = gtable_add_cols(table, spacing, col)
    )
    row <- row + as.numeric(side == "bottom")
    col <- col + as.numeric(side == "right")
  }

  table <- switch(
    side,
    bottom = , top = gtable_add_rows(table, max_height(grobs), row),
    left = , right = gtable_add_cols(table, max_width(grobs),  col)
  )
  name <- paste(name, seq_along(grobs), sep = "-")
  row  <- row + as.numeric(side %in% c("top", "bottom"))
  col  <- col + as.numeric(side %in% c("left", "right"))
  gtable_add_grob(table, grobs, t = row, l = col, name = name, z = z, clip = clip)
}
