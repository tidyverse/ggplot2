#' Lay out panels in a grid.
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

  facet(
    rows = rows, cols = cols, margins = margins, shrink = shrink,
    free = free, space_free = space_free, labeller = labeller,
    as.table = as.table, switch = switch, drop = drop,
    subclass = "grid"
  )
}


#' @export
facet_train_layout.grid <- function(facet, data) {
  layout <- layout_grid(data, facet$rows, facet$cols, facet$margins,
    drop = facet$drop, as.table = facet$as.table)

  # Relax constraints, if necessary
  layout$SCALE_X <- if (facet$free$x) layout$COL else 1L
  layout$SCALE_Y <- if (facet$free$y) layout$ROW else 1L

  layout
}


#' @export
facet_map_layout.grid <- function(facet, data, layout) {
  locate_grid(data, layout, facet$rows, facet$cols, facet$margins)
}

#' @export
facet_render.grid <- function(facet, panel, coord, theme, geom_grobs) {
  axes <- facet_axes(facet, panel, coord, theme)
  strips <- facet_strips(facet, panel, theme)
  panels <- facet_panels(facet, panel, coord, theme, geom_grobs)

  # adjust the size of axes to the size of panel
  axes$l$heights <- panels$heights
  axes$b$widths <- panels$widths

  # adjust the size of the strips to the size of the panels
  strips$r$heights <- panels$heights
  strips$t$widths <- panels$widths

  # Check if switch is consistent with grid layout
  switch_x <- !is.null(facet$switch) && facet$switch %in% c("both", "x")
  switch_y <- !is.null(facet$switch) && facet$switch %in% c("both", "y")
  if (switch_x && length(strips$t) == 0) {
    facet$switch <- if (facet$switch == "both") "y" else NULL
    switch_x <- FALSE
    warning("Cannot switch x axis strips as they do not exist", call. = FALSE)
  }
  if (switch_y && length(strips$r) == 0) {
    facet$switch <- if (facet$switch == "both") "x" else NULL
    switch_y <- FALSE
    warning("Cannot switch y axis strips as they do not exist", call. = FALSE)
  }


  # Combine components into complete plot
  if (is.null(facet$switch)) {
    top <- strips$t
    top <- gtable_add_cols(top, strips$r$widths)
    top <- gtable_add_cols(top, axes$l$widths, pos = 0)

    center <- cbind(axes$l, panels, strips$r, z = c(2, 1, 3))
    bottom <- axes$b
    bottom <- gtable_add_cols(bottom, strips$r$widths)
    bottom <- gtable_add_cols(bottom, axes$l$widths, pos = 0)

    complete <- rbind(top, center, bottom, z = c(1, 2, 3))

  } else {
    # Add padding between the switched strips and the axes
    padding <- convertUnit(theme$strip.switch.pad.grid, "cm")

    if (switch_x) {
      t_heights <- c(padding, strips$t$heights)
      gt_t <- gtable(widths = strips$t$widths, heights = unit(t_heights, "cm"))
      gt_t <- gtable_add_grob(gt_t, strips$t, name = strips$t$name, clip = "off",
        t = 1, l = 1, b = -1, r = -1)
    }
    if (switch_y) {
      r_widths <- c(strips$r$widths, padding)
      gt_r <- gtable(widths = unit(r_widths, "cm"), heights = strips$r$heights)
      gt_r <- gtable_add_grob(gt_r, strips$r, name = strips$r$name, clip = "off",
        t = 1, l = 1, b = -1, r = -1)
    }

    # Combine plot elements according to strip positions
    if (switch_x && switch_y) {
      center <- cbind(gt_r, axes$l, panels, z = c(3, 2, 1))

      bottom <- rbind(axes$b, gt_t)
      bottom <- gtable_add_cols(bottom, axes$l$widths, pos = 0)
      bottom <- gtable_add_cols(bottom, gt_r$widths, pos = 0)

      complete <- rbind(center, bottom, z = c(1, 2))
    } else if (switch_x) {
      center <- cbind(axes$l, panels, strips$r, z = c(2, 1, 3))

      bottom <- rbind(axes$b, gt_t)
      bottom <- gtable_add_cols(bottom, strips$r$widths)
      bottom <- gtable_add_cols(bottom, axes$l$widths, pos = 0)

      complete <- rbind(center, bottom, z = c(1, 2))
    } else if (switch_y) {
      top <- strips$t
      top <- gtable_add_cols(top, axes$l$widths, pos = 0)
      top <- gtable_add_cols(top, gt_r$widths, pos = 0)

      center <- cbind(gt_r, axes$l, panels, z = c(3, 2, 1))
      bottom <- axes$b
      bottom <- gtable_add_cols(bottom, axes$l$widths, pos = 0)
      bottom <- gtable_add_cols(bottom, gt_r$widths, pos = 0)

      complete <- rbind(top, center, bottom, z = c(1, 2, 3))
    } else {
      stop("`switch` must be either NULL, 'both', 'x', or 'y'",
        call. = FALSE)
    }
  }

  complete$respect <- panels$respect
  complete$name <- "layout"
  bottom <- axes$b

  complete
}

#' @export
facet_strips.grid <- function(facet, panel, theme) {
  col_vars <- unique(panel$layout[names(facet$cols)])
  row_vars <- unique(panel$layout[names(facet$rows)])

  # Adding labels metadata, useful for labellers
  attr(col_vars, "type") <- "cols"
  attr(col_vars, "facet") <- "grid"
  attr(row_vars, "type") <- "rows"
  attr(row_vars, "facet") <- "grid"

  dir <- list(r = "r", t = "t")
  if (!is.null(facet$switch) && facet$switch %in% c("both", "x")) {
    dir$t <- "b"
  }
  if (!is.null(facet$switch) && facet$switch %in% c("both", "y")) {
    dir$r <- "l"
  }

  strips <- list(
    r = build_strip(panel, row_vars, facet$labeller,
      theme, dir$r, switch = facet$switch),
    t = build_strip(panel, col_vars, facet$labeller,
      theme, dir$t, switch = facet$switch)
  )

  Map(function(strip, side) {
    if (side %in% c("t", "b")) {
      gtable_add_col_space(strip, theme$panel.margin.x %||% theme$panel.margin)
    } else {
      gtable_add_row_space(strip, theme$panel.margin.y %||% theme$panel.margin)
    }
  }, strips, dir)
}

#' @export
facet_axes.grid <- function(facet, panel, coord, theme) {
  axes <- list()

  # Horizontal axes
  cols <- which(panel$layout$ROW == 1)
  grobs <- lapply(panel$ranges[cols], coord$render_axis_h, theme = theme)
  axes$b <- gtable_add_col_space(gtable_row("axis-b", grobs),
    theme$panel.margin.x %||% theme$panel.margin)

  # Vertical axes
  rows <- which(panel$layout$COL == 1)
  grobs <- lapply(panel$ranges[rows], coord$render_axis_v, theme = theme)
  axes$l <- gtable_add_row_space(gtable_col("axis-l", grobs),
    theme$panel.margin.y %||% theme$panel.margin)

  axes
}

#' @export
facet_panels.grid <- function(facet, panel, coord, theme, geom_grobs) {

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !facet$free$x && !facet$free$y) {
    aspect_ratio <- coord$aspect(panel$ranges[[1]])
  }
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  # Add background and foreground to panels
  panels <- panel$layout$PANEL
  ncol <- max(panel$layout$COL)
  nrow <- max(panel$layout$ROW)

  panel_grobs <- lapply(panels, function(i) {
    fg <- coord$render_fg(panel$ranges[[i]], theme)
    bg <- coord$render_bg(panel$ranges[[i]], theme)

    geom_grobs <- lapply(geom_grobs, `[[`, i)

    if (theme$panel.ontop) {
      panel_grobs <- c(geom_grobs, list(bg), list(fg))
    } else {
      panel_grobs <- c(list(bg), geom_grobs, list(fg))
    }

    gTree(children = do.call("gList", panel_grobs))
  })

  panel_matrix <- matrix(panel_grobs, nrow = nrow, ncol = ncol, byrow = TRUE)

  # @kohske
  # Now size of each panel is calculated using PANEL$ranges, which is given by
  # coord_train called by train_range.
  # So here, "scale" need not to be referred.
  #
  # In general, panel has all information for building facet.
  if (facet$space_free$x) {
    ps <- panel$layout$PANEL[panel$layout$ROW == 1]
    widths <- vapply(ps, function(i) diff(panel$ranges[[i]]$x.range), numeric(1))
    panel_widths <- unit(widths, "null")
  } else {
    panel_widths <- rep(unit(1, "null"), ncol)
  }
  if (facet$space_free$y) {
    ps <- panel$layout$PANEL[panel$layout$COL == 1]
    heights <- vapply(ps, function(i) diff(panel$ranges[[i]]$y.range), numeric(1))
    panel_heights <- unit(heights, "null")
  } else {
    panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow)
  }

  panels <- gtable_matrix("panel", panel_matrix,
    panel_widths, panel_heights, respect = respect)
  panels <- gtable_add_col_space(panels, theme$panel.margin.x %||% theme$panel.margin)
  panels <- gtable_add_row_space(panels, theme$panel.margin.y %||% theme$panel.margin)

  panels
}

#' @export
facet_vars.grid <- function(facet) {
  paste(lapply(list(facet$rows, facet$cols), paste, collapse = ", "),
    collapse = " ~ ")
}
