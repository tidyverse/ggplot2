#' @include facet-.r
NULL

#' Lay out panels in a grid
#'
#' \code{facet_grid} forms a matrix of panels defined by row and column
#' facetting variables. It is most useful when you have two discrete
#' variables, and all combinations of the variables exist in the data.
#'
#' @param facets a formula with the rows (of the tabular display) on the LHS
#'   and the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this dimension
#'   (either row or column). The formula can also be provided as a string
#'   instead of a classical formula object
#' @param margins either a logical value or a character
#'   vector. Margins are additional facets which contain all the data
#'   for each of the possible values of the faceting variables. If
#'   \code{FALSE}, no additional facets are included (the
#'   default). If \code{TRUE}, margins are included for all faceting
#'   variables. If specified as a character vector, it is the names of
#'   variables for which margins are to be created.
#' @param scales Are scales shared across all facets (the default,
#'   \code{"fixed"}), or do they vary across rows (\code{"free_x"}),
#'   columns (\code{"free_y"}), or both rows and columns (\code{"free"})
#' @param space If \code{"fixed"}, the default, all panels have the same size.
#'   If \code{"free_y"} their height will be proportional to the length of the
#'   y scale; if \code{"free_x"} their width will be proportional to the
#'  length of the x scale; or if \code{"free"} both height and width will
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes one data frame of labels and
#'   returns a list or data frame of character vectors. Each input
#'   column corresponds to one factor. Thus there will be more than
#'   one with formulae of the type \code{~cyl + am}. Each output
#'   column gets displayed as one separate line in the strip
#'   label. This function should inherit from the "labeller" S3 class
#'   for compatibility with \code{\link{labeller}()}. See
#'   \code{\link{label_value}} for more details and pointers to other
#'   options.
#' @param as.table If \code{TRUE}, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If \code{FALSE}, the
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param switch By default, the labels are displayed on the top and
#'   right of the plot. If \code{"x"}, the top labels will be
#'   displayed to the bottom. If \code{"y"}, the right-hand side
#'   labels will be displayed to the left. Can also be set to
#'   \code{"both"}.
#' @param shrink If \code{TRUE}, will shrink scales to fit output of
#'   statistics, not raw data. If \code{FALSE}, will be range of raw data
#'   before statistical summary.
#' @param drop If \code{TRUE}, the default, all factor levels not used in the
#'   data will automatically be dropped. If \code{FALSE}, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, cty)) + geom_point()
#'
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(drv ~ .)
#' p + facet_grid(drv ~ cyl)
#'
#' # To change plot order of facet grid,
#' # change the order of variable levels with factor()
#'
#' # If you combine a facetted dataset with a dataset that lacks those
#' # facetting variables, the data will be repeated across the missing
#' # combinations:
#' df <- data.frame(displ = mean(mpg$displ), cty = mean(mpg$cty))
#' p +
#'   facet_grid(. ~ cyl) +
#'   geom_point(data = df, colour = "red", size = 2)
#'
#' # Free scales -------------------------------------------------------
#' # You can also choose whether the scales should be constant
#' # across all panels (the default), or whether they should be allowed
#' # to vary
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point()
#'
#' mt + facet_grid(. ~ cyl, scales = "free")
#'
#' # If scales and space are free, then the mapping between position
#' # and values in the data will be the same across all panels. This
#' # is particularly useful for categorical axes
#' ggplot(mpg, aes(drv, model)) +
#'   geom_point() +
#'   facet_grid(manufacturer ~ ., scales = "free", space = "free") +
#'   theme(strip.text.y = element_text(angle = 0))
#'
#' # Facet labels ------------------------------------------------------
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p
#'
#' # label_both() displays both variable name and value
#' p + facet_grid(vs ~ cyl, labeller = label_both)
#'
#' # label_parsed() parses text into mathematical expressions, see ?plotmath
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "sqrt(x, y)"))
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   facet_grid(. ~ cyl2, labeller = label_parsed)
#'
#' # label_bquote() makes it easy to construct math expressions
#' p + facet_grid(. ~ vs, labeller = label_bquote(cols = alpha ^ .(vs)))
#'
#' # The facet strips can be displayed near the axes with switch
#' data <- transform(mtcars,
#'   am = factor(am, levels = 0:1, c("Automatic", "Manual")),
#'   gear = factor(gear, levels = 3:5, labels = c("Three", "Four", "Five"))
#' )
#' p <- ggplot(data, aes(mpg, disp)) + geom_point()
#' p + facet_grid(am ~ gear, switch = "both")
#' # It looks better without boxes around the strips
#' p + facet_grid(am ~ gear, switch = "both") +
#'   theme(strip.background = element_blank())
#'
#' # Margins ----------------------------------------------------------
#' \donttest{
#' # Margins can be specified by logically (all yes or all no) or by specific
#' # variables as (character) variable names
#' mg <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' mg + facet_grid(vs + am ~ gear)
#' mg + facet_grid(vs + am ~ gear, margins = TRUE)
#' mg + facet_grid(vs + am ~ gear, margins = "am")
#' # when margins are made over "vs", since the facets for "am" vary
#' # within the values of "vs", the marginal facet for "vs" is also
#' # a margin over "am".
#' mg + facet_grid(vs + am ~ gear, margins = "vs")
#' mg + facet_grid(vs + am ~ gear, margins = "gear")
#' mg + facet_grid(vs + am ~ gear, margins = c("gear", "am"))
#' }
#' @importFrom plyr as.quoted
facet_grid <- function(facets, margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE) {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }

  # Facets can either be a formula, a string, or a list of things to be
  # convert to quoted
  if (is.character(facets)) {
    facets <- stats::as.formula(facets)
  }
  if (is.formula(facets)) {
    lhs <- function(x) if (length(x) == 2) NULL else x[-3]
    rhs <- function(x) if (length(x) == 2) x else x[-2]

    rows <- as.quoted(lhs(facets))
    rows <- rows[!sapply(rows, identical, as.name("."))]
    cols <- as.quoted(rhs(facets))
    cols <- cols[!sapply(cols, identical, as.name("."))]
  }
  if (is.list(facets)) {
    rows <- as.quoted(facets[[1]])
    cols <- as.quoted(facets[[2]])
  }
  if (length(rows) + length(cols) == 0) {
    stop("Must specify at least one variable to facet by", call. = FALSE)
  }

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  ggproto(NULL, FacetGrid,
    shrink = shrink,
    params = list(rows = rows, cols = cols, margins = margins,
      free = free, space_free = space_free, labeller = labeller,
      as.table = as.table, switch = switch, drop = drop)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetGrid <- ggproto("FacetGrid", Facet,
  shrink = TRUE,

  compute_layout = function(data, params) {
    rows <- as.quoted(params$rows)
    cols <- as.quoted(params$cols)

    base_rows <- combine_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
      base_rows[] <- lapply(base_rows, rev_order)
    }
    base_cols <- combine_vars(data, params$plot_env, cols, drop = params$drop)
    base <- df.grid(base_rows, base_cols)

    # Add margins
    base <- reshape2::add_margins(base, list(names(rows), names(cols)), params$margins)
    # Work around bug in reshape2
    base <- unique(base)

    # Create panel info dataset
    panel <- plyr::id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))

    rows <- if (is.null(names(rows))) 1L else plyr::id(base[names(rows)], drop = TRUE)
    cols <- if (is.null(names(cols))) 1L else plyr::id(base[names(cols)], drop = TRUE)

    panels <- data.frame(PANEL = panel, ROW = rows, COL = cols, base,
      check.names = FALSE, stringsAsFactors = FALSE)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    panels$SCALE_X <- if (params$free$x) panels$COL else 1L
    panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

    panels
  },
  map_data = function(data, layout, params) {
    if (empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }

    rows <- as.quoted(params$rows)
    cols <- as.quoted(params$cols)
    vars <- c(names(rows), names(cols))

    # Compute facetting values and add margins
    margin_vars <- list(intersect(names(rows), names(data)),
      intersect(names(cols), names(data)))
    data <- reshape2::add_margins(data, margin_vars, params$margins)

    facet_vals <- eval_facet_vars(c(rows, cols), data, params$plot_env)

    # If any facetting variables are missing, add them in by
    # duplicating the data
    missing_facets <- setdiff(vars, names(facet_vals))
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])

      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))

      data <- plyr::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- plyr::unrowname(cbind(
        facet_vals[data_rep, ,  drop = FALSE],
        to_add[facet_rep, , drop = FALSE]))
    }

    # Add PANEL variable
    if (nrow(facet_vals) == 0) {
      # Special case of no facetting
      data$PANEL <- NO_PANEL
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)

      keys <- plyr::join.keys(facet_vals, layout, by = vars)

      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data[order(data$PANEL), , drop = FALSE]
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    cols <- which(layout$ROW == 1)
    rows <- which(layout$COL == 1)
    axes <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)

    col_vars <- unique(layout[names(params$cols)])
    row_vars <- unique(layout[names(params$rows)])
    # Adding labels metadata, useful for labellers
    attr(col_vars, "type") <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type") <- "rows"
    attr(row_vars, "facet") <- "grid"
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)

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
      panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow)
    }

    panel_table <- gtable_matrix("layout", panel_table,
      panel_widths, panel_heights, respect = respect, clip = "on", z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

    panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$top), 0)
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$bottom), -1)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$left), 0)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$right), -1)
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
        if (inside_x) {
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
        if (inside_x) {
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
        if (inside_y) {
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
        if (inside_y) {
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
  }
)

# Helpers -----------------------------------------------------------------

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique(x))
  }
}
