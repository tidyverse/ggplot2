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
#' @export
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
      as.table = as.table, switch = switch, drop = drop)
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
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      layout[] <- lapply(layout[], as.factor)

      keys <- join_keys(facet_vals, layout, by = vars)

      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      cli::cli_abort("{.fn {snake_class(coord)}} doesn't support free scales.")
    }

    cols <- which(layout$ROW == 1)
    rows <- which(layout$COL == 1)
    axes <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)

    col_vars <- unique0(layout[names(params$cols)])
    row_vars <- unique0(layout[names(params$rows)])
    # Adding labels metadata, useful for labellers
    attr(col_vars, "type") <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type") <- "rows"
    attr(row_vars, "facet") <- "grid"
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)

    aspect_ratio <- theme$aspect.ratio
    if (!is.null(aspect_ratio) && (params$space_free$x || params$space_free$y)) {
      cli::cli_abort("Free scales cannot be mixed with a fixed aspect ratio.")
    }
    if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    panel_table <- matrix(panels, nrow = nrow, ncol = ncol, byrow = TRUE)

    # @kohske
    # Now size of each panel is calculated using PANEL$ranges, which is given by
    # coord_train called by train_range.
    # So here, "scale" need not to be referred.
    #
    # In general, panel has all information for building facet.
    if (params$space_free$x) {
      ps <- layout$PANEL[layout$ROW == 1]
      widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
      panel_widths <- unit(widths, "null")
    } else {
      panel_widths <- rep(unit(1, "null"), ncol)
    }
    if (params$space_free$y) {
      ps <- layout$PANEL[layout$COL == 1]
      heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
      panel_heights <- unit(heights, "null")
    } else {
      panel_heights <- rep(unit(1 * abs(aspect_ratio), "null"), nrow)
    }

    panel_table <- gtable_matrix("layout", panel_table,
      panel_widths, panel_heights, respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(nrow), ncol), '-', rep(seq_len(ncol), each = nrow))

    panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$top),     0)
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$bottom), -1)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$left),     0)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$right),   -1)
    panel_pos_col <- panel_cols(panel_table)
    panel_pos_rows <- panel_rows(panel_table)

    panel_table <- gtable_add_grob(panel_table, axes$x$top, 1, panel_pos_col$l, clip = "off", name = paste0("axis-t-", seq_along(axes$x$top)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$x$bottom, -1, panel_pos_col$l, clip = "off", name = paste0("axis-b-", seq_along(axes$x$bottom)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$y$left, panel_pos_rows$t, 1, clip = "off", name = paste0("axis-l-", seq_along(axes$y$left)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$y$right, panel_pos_rows$t, -1, clip = "off", name = paste0("axis-r-", seq_along(axes$y$right)), z= 3)

    # Add strips
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
    inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
    strip_padding <- convertUnit(theme$strip.switch.pad.grid, "cm")
    panel_pos_col <- panel_cols(panel_table)
    if (switch_x) {
      if (!is.null(strips$x$bottom)) {
        if (inside_x || all(vapply(axes$x$bottom, is.zero, logical(1)))) {
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -2)
          panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -2, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        } else {
          panel_table <- gtable_add_rows(panel_table, strip_padding, -1)
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -1)
          panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$x$top)) {
        if (inside_x || all(vapply(axes$x$top, is.zero, logical(1)))) {
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 1)
          panel_table <- gtable_add_grob(panel_table, strips$x$top, 2, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        } else {
          panel_table <- gtable_add_rows(panel_table, strip_padding, 0)
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 0)
          panel_table <- gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        }
      }
    }
    panel_pos_rows <- panel_rows(panel_table)
    if (switch_y) {
      if (!is.null(strips$y$left)) {
        if (inside_y || all(vapply(axes$y$left, is.zero, logical(1)))) {
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 1)
          panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 2, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        } else {
          panel_table <- gtable_add_cols(panel_table, strip_padding, 0)
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 0)
          panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$y$right)) {
        if (inside_y || all(vapply(axes$y$right, is.zero, logical(1)))) {
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -2)
          panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -2, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        } else {
          panel_table <- gtable_add_cols(panel_table, strip_padding, -1)
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -1)
          panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        }
      }
    }
    panel_table
  },
  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  }
)

# Helpers -----------------------------------------------------------------

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique0(x))
  }
}
