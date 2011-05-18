#' Wrap a 1d ribbon of panels into 2d.
#' 
#' @name facet_wrap
#' @param nrow number of rows
#' @param ncol number of columns
#' @param facet formula specifying variables to facet by
#' @param scales should scales be fixed, free, or free in one dimension
#'   (\code{free_x}, \code{free_y})
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
#'   xlim(0, 2) + stat_binhex(na.rm = TRUE) + opts(aspect.ratio = 1)
#' d + facet_wrap(~ color)
#' d + facet_wrap(~ color, ncol = 1)
#' d + facet_wrap(~ color, ncol = 4)
#' d + facet_wrap(~ color, nrow = 1)
#' d + facet_wrap(~ color, nrow = 3)
#' 
#' # Using multiple variables continues to wrap the long ribbon of 
#' # plots into 2d - the ribbon just gets longer
#' # d + facet_wrap(~ color + cut)
#' 
#' # You can choose to keep the scales constant across all panels
#' # or vary the x scale, the y scale or both:
#' p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
#' p + facet_wrap(~ color)
#' p + facet_wrap(~ color, scales = "free_y")
#' 
#' p <- qplot(displ, hwy, data = mpg)
#' p + facet_wrap(~ cyl)
#' p + facet_wrap(~ cyl, scales = "free") 
#' 
#' # Add data that does not contain all levels of the faceting variables
#' cyl6 <- subset(mpg, cyl == 6)
#' p + geom_point(data = cyl6, colour = "red", size = 1) + 
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") + 
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = NULL), colour = "red") + 
#'   facet_wrap(~ cyl)
#' 
#' # By default, any empty factor levels will be dropped
#' mpg$cyl2 <- factor(mpg$cyl, levels = c(2, 4, 5, 6, 8, 10))
#' qplot(displ, hwy, data = mpg) + facet_wrap(~ cyl2)
#' # Use drop = FALSE to force their inclusion
#' qplot(displ, hwy, data = mpg) + facet_wrap(~ cyl2, drop = FALSE)
FacetWrap <- proto(Facet, {
  objname <- "wrap"

  new <- function(., facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, as.table = TRUE, drop = TRUE) {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
    
    .$proto(
      facets = as.quoted(facets), free = free, shrink = shrink,
      as.table = as.table, drop = drop,
      ncol = ncol, nrow = nrow
    )
  }
  
  train <- function(., data) { 
    panels <- layout_wrap(data, .$facets, .$nrow, .$ncol, .$drop)
    
    # Add scale identification
    panels$SCALE_X <- if (.$free$x) seq_len(nrow(panels)) else 1
    panels$SCALE_Y <- if (.$free$y) seq_len(nrow(panels)) else 1
    
    .$panel_info <- panels
    invisible(NULL)
  }
  
  map_layer <- function(., data) {
    locate_wrap(data, .$panel_info, .$facets)
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., panels_grob, coord, theme) {
    coord_details <- llply(.$panel_info$PANEL, function(i) {
      coord$compute_ranges(.$panel_scales(i))
    })

    browser()
    axes <- .$build_axes(coord, coord_details, theme)
    strips <- .$build_strips(coord_details, theme)
    panels <- .$build_panels(panels_grob, coord, coord_details, theme)
    
    # How to think about facet wrap:
    #  * vector of panels
    #  * every panel has strips (strip_pos) and axes (axis_pos)
    #  * if scales fixed, most axes empty
    #  * combine panels, strips and axes, then wrap into 2d
    #  * finally: add title, labels and legend

    top <- (strips$t$clone())$
      add_cols(strips$r$widths)$
      add_cols(axes$l$widths, pos = 0)
    centre <- (axes$l$clone())$cbind(panels)$cbind(strips$r)
    bottom <- (axes$b$clone())$
      add_cols(strips$r$widths)$
      add_cols(axes$l$widths, pos = 0)
      
    complete <- centre$clone()$
      rbind(top, pos = 0)$
      rbind(bottom)
    complete$respect <- panels$respect
    complete$name <- "layout"
    
    complete

  

    aspect_ratio <- theme$aspect.ratio
    
    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    if (is.null(aspect_ratio) && !.$free$x && !.$free$y) {
      xscale <- .$scales$x[[1]]
      yscale <- .$scales$y[[1]]
      ranges <- coord$compute_ranges(list(x = xscale, y = yscale))
      aspect_ratio <- coord$compute_aspect(ranges)
    }
    
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
        
    n <- length(.$scales$x)

    axes_h <- matrix(list(), nrow = 1, ncol = n)
    axes_v <- matrix(list(), nrow = 1, ncol = n)
    panels <- matrix(list(), nrow = 1, ncol = n)

    for (i in seq_len(n)) {
      scales <- list(
        x = .$scales$x[[i]]$clone(), 
        y = .$scales$y[[i]]$clone()
      ) 
      details <- coord$compute_ranges(scales)
      axes_h[[1, i]] <- coord$guide_axis_h(details, theme)
      axes_v[[1, i]] <- coord$guide_axis_v(details, theme)

      fg <- coord$guide_foreground(details, theme)
      bg <- coord$guide_background(details, theme)
      name <- paste("panel", i, sep = "_")
      panels[[1,i]] <- ggname(name, grobTree(bg, panels_grob[[1, i]], fg))
    }
    


    # Create a grid of interwoven strips and panels
    panelsGrid <- grobGrid(
      "panel", panels, nrow = nrow, ncol = ncol,
      heights = 1 * aspect_ratio, widths = 1,
      as.table = .$as.table, respect = respect
    )

    strips <- .$labels_default(.$facet_levels, theme)
    strips_height <- max(do.call("unit.c", llply(strips, grobHeight)))
    stripsGrid <- grobGrid(
      "strip", strips, nrow = nrow, ncol = ncol,
      heights = convertHeight(strips_height, "cm"),
      widths = 1,
      as.table = .$as.table
    )
    
    axis_widths <- max(do.call("unit.c", llply(axes_v, grobWidth)))
    axis_widths <- convertWidth(axis_widths, "cm")
    if (.$free$y) {
      axesvGrid <- grobGrid(
        "axis_v", axes_v, nrow = nrow, ncol = ncol, 
        widths = axis_widths, 
        as.table = .$as.table, clip = "off"
      )
    } else { 
      # When scales are not free, there is only really one scale, and this
      # should be shown only in the first column
      axesvGrid <- grobGrid(
        "axis_v", rep(axes_v[1], nrow), nrow = nrow, ncol = 1,
        widths = axis_widths[1], 
        as.table = .$as.table, clip = "off")
      if (ncol > 1) {
        axesvGrid <- cbind(axesvGrid, 
          spacer(nrow, ncol - 1, unit(0, "cm"), unit(1, "null")))
        
      }
    }
    
    axis_heights <- max(do.call("unit.c", llply(axes_h, grobHeight)))
    axis_heights <- convertHeight(axis_heights, "cm")
    if (.$free$x) {
      axeshGrid <- grobGrid(
        "axis_h", axes_h, nrow = nrow, ncol = ncol, 
        heights = axis_heights, 
        as.table = .$as.table, clip = "off"
      )
    } else {
      # When scales are not free, there is only really one scale, and this
      # should be shown only in the bottom row
      axeshGrid <- grobGrid(
        "axis_h", rep(axes_h[1], ncol), nrow = 1, ncol = ncol,
        heights = axis_heights[1], 
        as.table = .$as.table, clip = "off")
      if (nrow > 1) { 
        axeshGrid <- rbind(
          spacer(nrow - 1, ncol, unit(1, "null"), unit(0, "cm")),
          axeshGrid
        )
      }
    }

    gap <- spacer(nrow, ncol, theme$panel.margin, theme$panel.margin)
    fill <- spacer(nrow, ncol, 0, 0, "null")
    
    all <- rweave(
      cweave(fill,      stripsGrid, fill),
      cweave(axesvGrid, panelsGrid, fill),
      cweave(fill,      axeshGrid,  fill),
      cweave(fill,      fill,       gap)
    )
    
    all
  }
  
  labels_default <- function(., labels_df, theme) {
    # Remove column giving panel number
    labels_df <- labels_df[, -ncol(labels_df), drop = FALSE]
    labels_df[] <- llply(labels_df, format, justify = "none")
    
    labels <- apply(labels_df, 1, paste, collapse=", ")

    llply(labels, ggstrip, theme = theme)
  }

  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", paste(names(.$facets), collapse = ", "), ")", sep="")
    if (newline) cat("\n")
  }
  
})

