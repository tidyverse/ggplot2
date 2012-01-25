#' Lay out panels in a grid.
#'
#' @param facets a formula with the rows (of the tabular display) on the LHS
#'   and the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this dimension
#'   (either row or column). The formula can also be provided as a string
#'   instead of a classical formula object
#' @param margins logical value, should marginal rows and columns be displayed
#' @param scales Are scales shared across all facets (the default,
#'   \code{"fixed"}), or do they vary across rows (\code{"free_x"}),
#'   columns (\code{"free_y"}), or both rows and columns (\code{"free"})
#' @param space If \code{"fixed"}, the default, all panels have the same size.
#'   If \code{"free_y"} their height will be proportional to the length of the
#'   y scale; if \code{"free_x"} their width will be proportional to the 
#'  length of the x scale; or if \code{"free"} both height and width will 
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes two arguments (\code{variable} and 
#'   \code{value}) and returns a string suitable for display in the facet
#'   strip. See \code{\link{label_value}} for more details and pointers
#'   to other options.
#' @param as.table If \code{TRUE}, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If \code{FALSE}, the 
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param shrink If \code{TRUE}, will shrink scales to fit output of
#'   statistics, not raw data. If \code{FALSE}, will be range of raw data
#'   before statistical summary.
#' @param drop If \code{TRUE}, the default, all factor levels not used in the
#'   data will automatically be dropped. If \code{FALSE}, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @export
#' @examples 
#' # faceting displays subsets of the data in different panels
#' p <- ggplot(diamonds, aes(carat, ..density..)) +
#'  geom_histogram(binwidth = 1)
#' 
#' # With one variable
#' p + facet_grid(. ~ cut)
#' p + facet_grid(cut ~ .)
#' 
#' # With two variables
#' p + facet_grid(clarity ~ cut)
#' p + facet_grid(cut ~ clarity)
#' p + facet_grid(cut ~ clarity, margins=TRUE)
#' 
#' # To change plot order of facet grid, 
#' # change the order of variable levels with factor()
#' diamonds$cut <- factor(diamonds$cut, levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))
#' # Repeat first example with new order
#' p <- ggplot(diamonds, aes(carat, ..density..)) +
#' geom_histogram(binwidth = 1)
#' p + facet_grid(. ~ cut)
#'
#' qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
#' qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )
#' 
#' # You can also use strings, which makes it a little easier
#' # when writing functions that generate faceting specifications
#' # p + facet_grid("cut ~ .")
#' 
#' # see also ?plotmatrix for the scatterplot matrix
#' 
#' # If there isn't any data for a given combination, that panel 
#' # will be empty
#' qplot(mpg, wt, data=mtcars) + facet_grid(cyl ~ vs)
#' 
#' # If you combine a facetted dataset with a dataset that lacks those
#' # facetting variables, the data will be repeated across the missing
#' # combinations:
#' p <- qplot(mpg, wt, data=mtcars, facets = vs ~ cyl)
#' 
#' df <- data.frame(mpg = 22, wt = 3)
#' p + geom_point(data = df, colour="red", size = 2)
#' 
#' df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
#' p + geom_point(data = df2, colour="red", size = 2)
#' 
#' df3 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
#' p + geom_point(data = df3, colour="red", size = 2)
#' 
#' 
#' # You can also choose whether the scales should be constant
#' # across all panels (the default), or whether they should be allowed
#' # to vary
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' 
#' mt + facet_grid(. ~ cyl, scales = "free")
#' # If scales and space are free, then the mapping between position
#' # and values in the data will be the same across all panels
#' mt + facet_grid(. ~ cyl, scales = "free", space = "free")
#' 
#' mt + facet_grid(vs ~ am, scales = "free")
#' mt + facet_grid(vs ~ am, scales = "free_x")
#' mt + facet_grid(vs ~ am, scales = "free_y")
#' mt + facet_grid(vs ~ am, scales = "free", space="free")
#' mt + facet_grid(vs ~ am, scales = "free", space="free_x")
#' mt + facet_grid(vs ~ am, scales = "free", space="free_y")
#' 
#' # You may need to set your own breaks for consitent display:
#' mt + facet_grid(. ~ cyl, scales = "free_x", space="free") + 
#'   scale_x_continuous(breaks = seq(10, 36, by = 2))
#' # Adding scale limits override free scales:
#' last_plot() + xlim(10, 15)
#' 
#' # Free scales are particularly useful for categorical variables
#' qplot(cty, model, data=mpg) + 
#'   facet_grid(manufacturer ~ ., scales = "free", space = "free")
#' # particularly when you reorder factor levels
#' mpg <- within(mpg, {
#'   model <- reorder(model, cty)
#'   manufacturer <- reorder(manufacturer, cty)
#' })
#' last_plot() %+% mpg + opts(strip.text.y = theme_text())
#'
#' # Use as.table to to control direction of horizontal facets, TRUE by default
#' h <- ggplot(mtcars, (aes(x = mpg, y = wt))) + geom_point()
#' h + facet_grid(cyl ~ vs)
#' h + facet_grid(cyl ~ vs, as.table = FALSE)
#' 
#' # Use labeller to control facet labels, label_value is default
#' h + facet_grid(cyl ~ vs, labeller = label_both)
#' # Using label_parsed, see ?plotmath for more options
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "sqrt(x, y)"))
#' k <- qplot(wt, mpg, data = mtcars) 
#' k + facet_grid(. ~ cyl2)
#' k + facet_grid(. ~ cyl2, labeller = label_parsed)
#' # For label_bquote the label value is x.
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(.(x) ^ .(x)))
facet_grid <- function(facets, margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, drop = TRUE) {
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
    facets <- as.formula(facets)
  }
  if (is.formula(facets)) {
    rows <- as.quoted(facets[[2]])
    rows <- rows[!sapply(rows, identical, as.name("."))]
    cols <- as.quoted(facets[[3]])
    cols <- cols[!sapply(cols, identical, as.name("."))]
  }
  if (is.list(facets)) {
    rows <- as.quoted(facets[[1]])
    cols <- as.quoted(facets[[2]])
  }
  if (length(rows) + length(cols) == 0) {
    stop("Must specify at least one variable to facet by", call. = FALSE)
  }
  
  facet(
    rows = rows, cols = cols, margins = margins, shrink = shrink,
    free = free, space_free = space_free, 
    labeller = labeller, as.table = as.table, drop = drop,
    subclass = "grid"
  )
}

#' @S3method facet_train_layout grid
facet_train_layout.grid <- function(facet, data) { 
  layout <- layout_grid(data, facet$rows, facet$cols, facet$margins,
    facet$drop)
    
  # Relax constraints, if necessary
  layout$SCALE_X <- if (facet$free$x) layout$COL else 1L
  layout$SCALE_Y <- if (facet$free$y) layout$ROW else 1L
  
  layout
}


#' @S3method facet_map_layout grid
facet_map_layout.grid <- function(facet, data, layout) {
  locate_grid(data, layout, facet$rows, facet$cols, facet$margins)
}

#' @S3method facet_render grid
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
  
  # Combine components into complete plot
  top <- strips$t
  top <- gtable_add_cols(top, strips$r$widths)
  top <- gtable_add_cols(top, axes$l$widths, pos = 0)
  
  center <- cbind(cbind(axes$l, panels), strips$r)
  bottom <- axes$b
  bottom <- gtable_add_cols(bottom, strips$r$widths)
  bottom <- gtable_add_cols(bottom, axes$l$widths, pos = 0)

  complete <- rbind(top, rbind(center, bottom))
  complete$respect <- panels$respect
  complete$name <- "layout"
  
  complete
}

#' @S3method facet_strips grid
facet_strips.grid <- function(facet, panel, theme) {
  col_vars <- unique(panel$layout[names(facet$cols)])
  row_vars <- unique(panel$layout[names(facet$rows)])

  list(
    r = build_strip(panel, row_vars, facet$labeller, theme, "r"),
    t = build_strip(panel, col_vars, facet$labeller, theme, "t")
  )
}

build_strip <- function(panel, label_df, labeller, theme, side = "right") {
  side <- match.arg(side, c("top", "left", "bottom", "right"))
  horizontal <- side %in% c("top", "bottom")
  labeller <- match.fun(labeller)
  
  # No labelling data, so return empty row/col
  if (empty(label_df)) {
    if (horizontal) {
      widths <- unit(rep(0, max(panel$layout$COL)), "null")
      return(layout_empty_row(widths))
    } else {
      heights <- unit(rep(0, max(panel$layout$ROW)), "null")
      return(layout_empty_col(heights))
    }
  }
  
  # Create matrix of labels
  labels <- matrix(list(), nrow = nrow(label_df), ncol = ncol(label_df))
  for (i in seq_len(ncol(label_df))) {
    labels[, i] <- labeller(names(label_df)[i], label_df[, i])
  }
  
  # Render as grobs
  grobs <- aaply(labels, c(1,2), ggstrip, theme = theme, 
    horizontal = horizontal, .drop = FALSE)
  
  # Create layout
  name <- paste("strip", side, sep = "-")
  if (horizontal) {
    grobs <- t(grobs)
    
    # Each row is as high as the highest and as a wide as the panel
    row_height <- function(row) max(laply(row, height_cm))
    heights <- unit(apply(grobs, 1, row_height), "cm")
    widths <- unit(rep(1, ncol(grobs)), "null")
  } else {
    # Each row is wide as the widest and as high as the panel
    col_width <- function(col) max(laply(col, width_cm))
    widths <- unit(apply(grobs, 2, col_width), "cm")
    heights <- unit(rep(1, nrow(grobs)), "null")
  }
  strips <- layout_matrix(name, grobs, heights = heights, widths = widths)
  
  if (horizontal) {
    gtable_add_col_space(strips, theme$panel.margin)
  } else {
    gtable_add_row_space(strips, theme$panel.margin)
  }
}

#' @S3method facet_axes grid
facet_axes.grid <- function(facet, panel, coord, theme) {
  axes <- list()

  # Horizontal axes
  cols <- which(panel$layout$ROW == 1)
  grobs <- lapply(panel$ranges[cols], coord_render_axis_h, 
    coord = coord, theme = theme)
  axes$b <- gtable_add_col_space(layout_row("axis-b", grobs),
    theme$panel.margin)

  # Vertical axes
  rows <- which(panel$layout$COL == 1)
  grobs <- lapply(panel$ranges[rows], coord_render_axis_v, 
    coord = coord, theme = theme)
  axes$l <- gtable_add_row_space(layout_col("axis-l", grobs),
    theme$panel.margin)

  axes
}

#' @S3method facet_panels grid
facet_panels.grid <- function(facet, panel, coord, theme, geom_grobs) {
  
  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !facet$free$x && !facet$free$y) {
    aspect_ratio <- coord_aspect(coord, panel$ranges[[1]])
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
    fg <- coord_render_fg(coord, panel$range[[i]], theme)
    bg <- coord_render_bg(coord, panel$range[[i]], theme)
    
    geom_grobs <- lapply(geom_grobs, "[[", i)
    panel_grobs <- c(list(bg), geom_grobs, list(fg))
    
    gTree(children = do.call("gList", panel_grobs))  
  })
  
  panel_matrix <- matrix(panel_grobs, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  size <- function(x) unit(diff(scale_dimension(x)), "null")
  
  if (facet$space_free$x) {
    x_scales <- panel$layout$SCALE_X[panel$layout$ROW == 1]
    panel_widths <- do.call("unit.c", llply(panel$x_scales, size))[x_scales]
  } else {
    panel_widths <- rep(unit(1, "null"), ncol)
  }
  if (facet$space_free$y) {
    y_scales <- panel$layout$SCALE_Y[panel$layout$COL == 1]
    panel_heights <- do.call("unit.c", llply(panel$y_scales, size))[y_scales]
  } else {
    panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow)
  }
  
  panels <- layout_matrix("panel", panel_matrix,
    panel_widths, panel_heights, respect = respect)
  panels <- gtable_add_col_space(panels, theme$panel.margin)
  panels <- gtable_add_row_space(panels, theme$panel.margin)
    
  panels
}

icon.grid <- function(.) {
  gTree(children = gList(
    rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
  ))
}  

#' @S3method facet_vars grid
facet_vars.grid <- function(facet) {
  paste(lapply(list(facet$rows, facet$cols), paste, collapse = ", "), 
    collapse = " ~ ")
}
