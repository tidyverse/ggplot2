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
    .$shape <- dlply(data[[1]], .$facets, nrow)
    dim(.$shape) <- c(1, length(.$shape))
  }
  
  stamp_data <- function(., data) {
    data.matrix <- dlply(add_group(data), .$facets)
    dim(data.matrix) <- c(1, length(data.matrix))
    data.matrix
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels_grob, coord, theme) {
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio)) aspect_ratio <- 1
    
    n <- length(.$scales$x)

    axes_h <- matrix(list(), nrow = 1, ncol = n)
    axes_v <- matrix(list(), nrow = 1, ncol = n)
    
    for(i in seq_len(n)) {
      axes_h[[1, i]] <- coord$guide_axes(.$scales$x[[i]], theme, "bottom")
      axes_v[[1, i]] <- coord$guide_axes(.$scales$y[[i]], theme, "left")
    }

    labels <- .$labels_default(.$shape, theme)
    dim(labels) <- c(1, length(labels))

    # Add background and foreground to panels
    panels <- matrix(list(), nrow = 1, ncol = n)
    
    for(i in seq_len(n)) {
      scales <- list(
        x = .$scales$x[[i]], 
        y = .$scales$y[[i]]
      )
      fg <- coord$guide_foreground(scales, theme)
      bg <- coord$guide_background(scales, theme)

      panels[[1,i]] <- ggname("panel", grobTree(bg, panels_grob[[1, i]], fg))
    }
    
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
      axes_v <- cbind(
        grobMatrix(rep(axes_v[1], nrow), nrow, 1, .$as.table),
        grobMatrix(list(nullGrob()), nrow, ncol - 1, .$as.table)
      )
    }
    axes_width <- grobColWidth(axes_v)
    
    axes_h <- grobMatrix(axes_h, nrow, ncol, .$as.table)
    if (.$free$x) {
      axes_h <- grobMatrix(axes_h, nrow, ncol, .$as.table)
    } else {
      axes_h <- rbind(
        grobMatrix(list(nullGrob()),        nrow - 1, ncol, .$as.table),
        grobMatrix(rep(axes_h[1], ncol), 1, ncol, .$as.table)
      )
    }
    axes_height <- grobRowHeight(axes_h)

    all <- cweave(
      rweave(gap,    axes_v, gap,    gap), 
      rweave(labels, panels, axes_h, gap), 
      rweave(gap,    gap,    gap,    gap)
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
    labels <- aaply(labels_df, 1, paste, collapse=", ")

    llply(labels, ggstrip, theme = theme)
  }
  
  create_viewports <- function(., guides, theme) {
    respect <- !is.null(theme$aspect_ratio)
    
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
  
  make_grobs <- function(., data, layers, cs) {
    lapply(seq_along(data), function(i) {
      layer <- layers[[i]]
      layerd <- data[[i]]
      grobs <- matrix(list(), nrow = nrow(layerd), ncol = ncol(layerd))

      for(i in seq_along(.$scales$x)) {
        scales <- list(
          x = .$scales$x[[i]], 
          y = .$scales$y[[i]]
        )
        grobs[[1, i]] <- layer$make_grob(layerd[[1, i]], scales, cs)
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
    scales = "should scales be fixed, free, or free in one dimension (free_x, free_y) "
  )

  
  
  examples <- function(.) {
    d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
      xlim(0, 2) + stat_binhex() + opts(aspect.ratio = 1)
    d + facet_wrap(~ color)
    d + facet_wrap(~ color, ncol = 4)
    d + facet_wrap(~ color, nrow = 4)
    
    # Using multiple variables continues to wrap the long ribbon of 
    # plots into 2d - the ribbon just gets longer
    d + facet_wrap(~ color + cut)

    # You can choose to keep the scales constant across all panels
    # or vary the x scale, the y scale or both:
    p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
    p + facet_wrap(~ color)
    p + facet_wrap(~ color, scales = "free_y")
    
    p <- qplot(displ, hwy, data = mpg)
    p + facet_wrap(~ cyl)
    p + facet_wrap(~ cyl, scales = "free") 
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", .$facets, ", ", .$margins, ")", sep="")
    if (newline) cat("\n")
  }
  
})


#X a <- matrix(1:10 * 2, ncol = 2)
#X b <- matrix(1:10 * 3, ncol = 2)
#X c <- matrix(1:10 * 5, ncol = 2)
rweave <- function(...) {
  matrices <- list(...)
  stopifnot(equal_dims(matrices))
  
  n <- nrow(matrices[[1]])
  p <- length(matrices)
  
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  do.call("rbind", matrices)[interleave, , drop = FALSE]
}

cunion <- function(a, b) {
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)
  
  cbind(a, b[setdiff(names(b), names(a))])
}

cweave <- function(...) {
  matrices <- list(...)
  stopifnot(equal_dims(matrices))
  
  n <- ncol(matrices[[1]])
  p <- length(matrices)

  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  do.call("cbind", matrices)[, interleave, drop = FALSE]
}

interleave <- function(...) {
  vectors <- list(...)
  
  # Check lengths 
  lengths <- unique(setdiff(laply(vectors, length), 1))
  if (length(lengths) == 0) lengths <- 1
  stopifnot(length(lengths) <= 1)
  
  # Replicate elements of length one up to correct length
  singletons <- laply(vectors, length) == 1
  vectors[singletons] <- llply(vectors[singletons], rep, lengths)
  
  # Interleave vectors
  n <- lengths
  p <- length(vectors)
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  unlist(vectors, recursive=FALSE)[interleave]
}

equal_dims <- function(matrices) {
  are.matrices <- laply(matrices, is.matrix)
  stopifnot(all(are.matrices))
  
  cols <- laply(matrices, ncol)
  rows <- laply(matrices, ncol)

  length(unique(cols) == 1) && length(unique(rows) == 1)
} 

grobRowHeight <- function(mat) {
  row_heights <- alply(mat, 1, function(x) llply(x, grobHeight))
  do.call("unit.c", llply(row_heights, splat(max)))  
}

grobColWidth <- function(mat) {
  col_widths <- alply(mat, 2, function(x) llply(x, grobWidth))
  do.call("unit.c", llply(col_widths, splat(max)))  
}

grobMatrix <- function(vec, nrow, ncol, as.table = FALSE) {
  mat <- c(vec, rep(list(nullGrob()), nrow * ncol - length(vec)))
  dim(mat) <- c(ncol, nrow)
  mat <- t(mat)
  if (!as.table) mat <- mat[rev(seq_len(nrow)), ]
  
  mat
}