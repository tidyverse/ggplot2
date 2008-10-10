FacetWrap <- proto(Facet, {
  new <- function(., facets = . ~ ., nrows = NULL, ncols = NULL, scales = "fixed") {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
    
    .$proto(
      facets = as.quoted(facets), free = free, 
      scales = NULL, 
      ncols = ncols, nrows = nrows
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
  add_guides <- function(., data, panels_grob, coordinates, theme) {
    n <- length(.$scales$x)
    
    axes_h <- matrix(list(), nrow = 1, ncol = n)
    axes_v <- matrix(list(), nrow = 1, ncol = n)

    for(i in seq_len(n)) {
      axes_h[[1, i]] <- coordinates$guide_axes(.$scales$x[[i]], theme, "bottom")
      axes_v[[1, i]] <- coordinates$guide_axes(.$scales$y[[i]], theme, "left")
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
      fg <- coordinates$guide_foreground(scales, theme)
      bg <- coordinates$guide_background(scales, theme)

      panels[[1,i]] <- ggname("panel", grobTree(bg, panels_grob[[1, i]], fg))
    }
    
    # Arrange 1d structure into a grid -------
    if (is.null(.$ncols) && is.null(.$nrows)) {
      ncols <- ceiling(sqrt(n))
      nrows <- ceiling(n / ncols)
    } else if (is.null(.$ncols)) {
      nrows <- .$nrows
      ncols <- ceiling(n / nrows)
    } else if (is.null(.$nrows)) {
      ncols <- .$ncols
      nrows <- ceiling(n / ncols)
    }
    stopifnot(nrows * ncols >= n)

    # Create a grid of interwoven strips and panels
    np <- nrows * ncols
    panels <- c(panels, rep(list(nullGrob()), np - n))
    dim(panels) <- c(ncols, nrows)
    panels <- t(panels)

    labels <- c(labels, rep(list(nullGrob()), np - n))
    dim(labels) <- c(ncols, nrows)
    labels <- t(labels)    
    
    labpanel <- rweave(labels, panels)

    axes_v <- axes_v[rep(1, nrows), 1, drop = FALSE]
    axes_v <- rweave(matrix(list(nullGrob()), nrow = nrows, ncol = 1), axes_v)
    
    axes_h <- axes_h[1, rep(1, ncols), drop = FALSE]    
    
    list(
      panel     = labpanel, 
      axis_v    = axes_v,
      axis_h    = axes_h
      # strip_h   = labels
    )
  }
  
  labels_default <- function(., gm, theme) {
    labels_df <- attr(gm, "split_labels")
    labels <- aaply(labels_df, 1, paste, collapse=", ")

    llply(labels, ggstrip, theme = theme)
  }
  
  create_viewports <- function(., guides, theme) {
    aspect_ratio <- theme$aspect.ratio
    respect <- !is.null(aspect_ratio)
    if (is.null(aspect_ratio)) aspect_ratio <- 1
    
    panel_widths <- rep(unit(1, "null"), ncol(guides$panel))
    
    odds <- seq(1, nrow(guides$panel), by = 2)
    labels <- guides$panel[odds, ]
    panels <- guides$panel[odds, ]
    
    row_heights <- alply(labels, 1, function(x) llply(x, grobHeight))
    label_height <- do.call("unit.c", llply(row_heights, splat(max)))
    panel_height <- unit(rep(1 * aspect_ratio, length(odds)), "null")
    
    panel_heights <- unit.c(label_height, panel_height)[rep(seq_along(label_height), each = 2) + c(0, length(label_height))]
    
    #rep(unit(1 * aspect_ratio, "null"), nrow(guides$panel))
    
    widths <- unit.c(
      do.call("max", llply(guides$axis_v, grobWidth)),
      panel_widths
    )
        
    heights <- unit.c(
      panel_heights,
      do.call("max", llply(guides$axis_h, grobHeight))
    )
    
    layout <- grid.layout(
      ncol = length(widths), widths = widths,
      nrow = length(heights), heights = heights,
      respect = respect
    )
    layout_vp <- viewport(layout=layout, name="panels")
    
    panel_rows <- nrow(guides$panel)
    panel_cols <- ncol(guides$panel)
    
    children_vp <- do.call("vpList", c(
      setup_viewports("axis_v",  guides$axis_v,  c(0, 0), "off"),
      setup_viewports("panel",   guides$panel,   c(0, 1)),
      setup_viewports("axis_h",  guides$axis_h,  c(panel_rows, 1), "off")
    ))
    
    vpTree(layout_vp, children_vp)
  }

  
  # Position scales ----------------------------------------------------------
  
  position_train <- function(., data, scales) {
    if (is.null(.$scales)) {
      fr <- .$free
      .$scales$x <- scales_list(scales$get_scales("x"), length(.$shape), fr$x)
      .$scales$y <- scales_list(scales$get_scales("y"), length(.$shape), fr$y)
    }

    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        .$scales$x[[i]]$train_df(l[[1, i]])
        .$scales$y[[i]]$train_df(l[[1, i]])
      }
    })
  }
  
  position_map <- function(., data, scales) {
    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        l[1, i] <- lapply(l[1, i], function(old) {
          new_x <- .$scales$x[[i]]$map_df(old)
          new_y <- .$scales$y[[i]]$map_df(old)
          
          cbind(
            new_x, 
            new_y,
            old[setdiff(names(old), c(names(new_x), names(new_y)))]
          )
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
  

  # Documentation ------------------------------------------------------------

  objname <- "wrap"
  desc <- "Wrap a 1d ribbon of panels into 2d."
  
  desc_params <- list(
    facets = "",
    margins = "logical value, should marginal rows and columns be displayed"
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
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", .$facets, ", ", .$margins, ")", sep="")
    if (newline) cat("\n")
  }
  
})

rweave <- function(a, b) {
  stopifnot(ncol(a) == ncol(b))
  stopifnot(nrow(a) == nrow(b))
  n <- nrow(a)

  rbind(a, b)[rep(1:n, each = 2) + c(0, n), , drop = FALSE]
}