FacetWrap <- proto(Facet, {
  new <- function(., facets, nrow = NULL, ncol = NULL, scales = "fixed", as.table = TRUE) {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
    
    .$proto(
      facets = as.quoted(facets), free = free, 
      scales = NULL, as.table = as.table,
      ncol = ncol, nrow = nrow
    )
  }
  
  conditionals <- function(.) {
    names(.$facets)
  }
  
  # Data shape
  
  initialise <- function(., data) {
    vars <- llply(data, function(df) {
      as.data.frame(eval.quoted(.$facets, df))
    })
    labels <- unique(do.call(rbind, vars))
    labels <- labels[do.call("order", labels), , drop = FALSE]
    n <- nrow(labels)
    
    .$shape <- matrix(NA, 1, n)
    attr(.$shape, "split_labels") <- labels
  }
  
  stamp_data <- function(., data) {
    data <- add_missing_levels(data, .$conditionals())
    lapply(data, function(df) {
      data.matrix <- dlply(add_group(df), .$facets, .drop = FALSE)
      data.matrix <- as.list(data.matrix)
      dim(data.matrix) <- c(1, length(data.matrix))
      data.matrix
    })
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels_grob, coord, theme) {
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio)) aspect_ratio <- 1
    
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

    labels <- .$labels_default(.$shape, theme)
    dim(labels) <- c(1, length(labels))
    
    # Arrange 1d structure into a grid -------
    if (is.null(.$ncol) && is.null(.$nrow)) {
      ncol <- ceiling(sqrt(n))
      nrow <- ceiling(n / ncol)
    } else if (is.null(.$ncol)) {
      nrow <- .$nrow
      ncol <- ceiling(n / nrow)
    } else if (is.null(.$nrow)) {
      ncol <- .$ncol
      nrow <- ceiling(n / ncol)
    }
    stopifnot(nrow * ncol >= n)

    # Create a grid of interwoven strips and panels
    panels <- grobMatrix(panels, nrow, ncol, .$as.table)
    panels_height <- list(unit(1 * aspect_ratio, "null"))
    panels_width <-  list(unit(1, "null"))

    labels <- grobMatrix(labels, nrow, ncol, .$as.table)
    labels_height <- grobRowHeight(labels)
    
    gap <- matrix(list(nullGrob()), ncol = ncol, nrow = nrow)
    
    if (.$free$y) {
      axes_v <- grobMatrix(axes_v, nrow, ncol, .$as.table)
    } else {
      axes_v <- grobMatrix(rep(axes_v[1], nrow), nrow, 1, .$as.table)
      if (ncol > 1) {
        empty <- grobMatrix(list(nullGrob()), nrow, ncol - 1, .$as.table)
        axes_v <- cbind(axes_v, empty)
      }
    }
    axes_width <- grobColWidth(axes_v)
    
    if (.$free$x) {
      axes_h <- grobMatrix(axes_h, nrow, ncol, .$as.table)
    } else {
      axes_h <- grobMatrix(rep(axes_h[1], ncol), 1, ncol, .$as.table)
      if (nrow > 1) {
        empty <- grobMatrix(list(nullGrob()), nrow - 1, ncol, .$as.table)
        axes_h <- rbind(empty, axes_h)
      }
    }
    axes_height <- grobRowHeight(axes_h)

    all <- rweave(
      cweave(gap,    labels, gap),
      cweave(axes_v, panels, gap),
      cweave(gap,    axes_h, gap),
      cweave(gap,    gap,    gap)
    )    
    
    margin <- list(theme$panel.margin)
    heights <- interleave(labels_height, panels_height, axes_height, margin)
    heights <- do.call("unit.c", heights)

    widths <- interleave(axes_width, panels_width, margin)
    widths <- do.call("unit.c", widths)
    
    list(
      panel   = all, 
      widths  = widths,
      heights = heights
    )
  }
  
  labels_default <- function(., gm, theme) {
    labels_df <- attr(gm, "split_labels")
    labels_df[] <- llply(labels_df, format, justify = "none")
    labels <- apply(labels_df, 1, paste, collapse=", ")

    llply(labels, ggstrip, theme = theme)
  }
  
  create_viewports <- function(., guides, theme) {
    respect <- !is.null(theme$aspect.ratio)
    layout <- grid.layout(
      ncol = length(guides$widths), widths = guides$widths,
      nrow = length(guides$heights), heights = guides$heights,
      respect = respect
    )
    layout_vp <- viewport(layout=layout, name="panels")
    
    children_vp <- vpList(
      setup_viewports("panel",   guides$panel, clip = "on")
    )
    
    vpTree(layout_vp, children_vp)
  }
  
  # Position scales ----------------------------------------------------------
  
  position_train <- function(., data, scales) {
    fr <- .$free
    if (is.null(.$scales$x) && scales$has_scale("x")) {
      .$scales$x <- scales_list(scales$get_scales("x"), length(.$shape), fr$x)
    }
    if (is.null(.$scales$y) && scales$has_scale("y")) {
      .$scales$y <- scales_list(scales$get_scales("y"), length(.$shape), fr$y)
    }

    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        .$scales$x[[i]]$train_df(l[[1, i]])
      }
      for(i in seq_along(.$scales$y)) {
        .$scales$y[[i]]$train_df(l[[1, i]])
      }
    })
  }
  
  position_map <- function(., data, scales) {
    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        l[1, i] <- lapply(l[1, i], function(old) {
          new <- .$scales$x[[i]]$map_df(old)
          if (!is.null(.$scales$y[[i]])) {
            new <- cbind(new, .$scales$y[[i]]$map_df(old))
          }
          
          
          cunion(new, old)
        }) 
      }
      l
    })
  }
  
  make_grobs <- function(., data, layers, coord) {
    lapply(seq_along(data), function(i) {
      layer <- layers[[i]]
      layerd <- data[[i]]
      grobs <- matrix(list(), nrow = nrow(layerd), ncol = ncol(layerd))

      for(i in seq_along(.$scales$x)) {
        scales <- list(
          x = .$scales$x[[i]]$clone(), 
          y = .$scales$y[[i]]$clone()
        )
        details <- coord$compute_ranges(scales)
        grobs[[1, i]] <- layer$make_grob(layerd[[1, i]], details, coord)
      }
      grobs
    })
  }
  
  calc_statistics <- function(., data, layers) {
    lapply(seq_along(data), function(i) {
      layer <- layers[[i]]
      layerd <- data[[i]]
      grobs <- matrix(list(), nrow = nrow(layerd), ncol = ncol(layerd))

      for(i in seq_along(.$scales$x)) {
        scales <- list(
          x = .$scales$x[[i]], 
          y = .$scales$y[[i]]
        )
        grobs[[1, i]] <- layer$calc_statistic(layerd[[1, i]], scales)
      }
      grobs
    })
  }
  

  # Documentation ------------------------------------------------------------

  objname <- "wrap"
  desc <- "Wrap a 1d ribbon of panels into 2d."
  
  desc_params <- list(
    nrow = "number of rows",
    ncol = "number of colums", 
    facet = "formula specifying variables to facet by",
    scales = "should scales be fixed, free, or free in one dimension (\\code{free_x}, \\code{free_y}) "
  )

  
  
  examples <- function(.) {
    d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
      xlim(0, 2) + stat_binhex(na.rm = TRUE) + opts(aspect.ratio = 1)
    d + facet_wrap(~ color)
    d + facet_wrap(~ color, ncol = 4)
    d + facet_wrap(~ color, nrow = 3)
    
    # Using multiple variables continues to wrap the long ribbon of 
    # plots into 2d - the ribbon just gets longer
    # d + facet_wrap(~ color + cut)

    # You can choose to keep the scales constant across all panels
    # or vary the x scale, the y scale or both:
    p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
    p + facet_wrap(~ color)
    p + facet_wrap(~ color, scales = "free_y")
    
    p <- qplot(displ, hwy, data = mpg)
    p + facet_wrap(~ cyl)
    p + facet_wrap(~ cyl, scales = "free") 
    
    # Add data that does not contain all levels of the faceting variables
    cyl6 <- subset(mpg, cyl == 6)
    p + geom_point(data = cyl6, colour = "red", size = 1) + 
      facet_wrap(~ cyl)
    p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") + 
      facet_wrap(~ cyl)
    
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", paste(names(.$facets), collapse = ", "), ")", sep="")
    if (newline) cat("\n")
  }
  
})

