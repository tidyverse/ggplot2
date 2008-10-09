FacetGrid <- proto(Facet, {
  new <- function(., facets = . ~ ., margins = FALSE, scales = "fixed", space = "fixed", free = list(x=T, y =F)) {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    space <- match.arg(scales, c("fixed", "free"))
    
    if (is.formula(facets)) facets <- deparse(facets) 
    .$proto(
      facets = facets, margins = margins,
      scales = scales, space = space, free = free,
      scales_x = NULL, scales_y = NULL
    )
  }
  
  conditionals <- function(.) {
    vars <- all.vars(as.formula(.$facets))
    setdiff(vars, c(".", "..."))
  }
  
  stamp_data <- function(., data) {
    data.matrix <- stamp(add_group(data), .$facets, force, 
      margins=.$margins, fill = list(NULL))
    
    force_matrix(data.matrix)
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels, coordinates, theme) {
    nr <- nrow(panels)
    nc <- ncol(panels)
    
    axes_h <- matrix(list(), nrow = 1, ncol = nc)
    for(i in seq_along(.$scales_x)) {
      s <- Scales$new()
      s$add(.$scales_x[[i]])
      s$add(.$scales_y[[1]])
      coordinates$train(s)
      
      axes_h[[1, i]] <- coordinates$guide_axis_x(theme)
    }
    
    axes_v <- matrix(list(), nrow = nr, ncol = 1)
    for(i in seq_along(.$scales_y)) {
      s <- Scales$new()
      s$add(.$scales_x[[1]])
      s$add(.$scales_y[[i]])
      coordinates$train(s)
      
      axes_v[[i, 1]] <- coordinates$guide_axis_y(theme)
    }    
    
    labels <- labels_default(.$shape, theme)

    # Add background and foreground to panels
    fg <- coordinates$guide_foreground(theme)
    bg <- coordinates$guide_background(theme)
    bg_empty <- theme_render(theme, "panel.empty")
    
    panels <- aaply(panels, 1:2, function(panel_grob) {
      if (is.null(panel_grob[[1]])) return(bf_empty) 
      grobTree(bg, panel_grob[[1]], fg)
    })
    dim(panels) <- c(nr, nc)

    list(
      panel     = panels, 
      axis_v    = axes_v,
      strip_v   = labels$v,
      axis_h    = axes_h,
      strip_h   = labels$h
    )
  }
  
  create_viewports <- function(., guides, theme) {
    aspect_ratio <- theme$aspect.ratio
    respect <- !is.null(aspect_ratio)
    if (is.null(aspect_ratio)) aspect_ratio <- 1
    
    widths <- unit.c(
      grobWidth(guides$axis_v[[1]]),
      rep(unit(1, "null"), ncol(guides$panel)),
      do.call("unit.c", lapply(guides$strip_v[1, ], grobWidth))
    )
    
    heights <- unit.c(
      do.call("unit.c", lapply(guides$strip_h[, 1], grobHeight)),
      rep(unit(1 * aspect_ratio, "null"), nrow(guides$panel)),
      grobHeight(guides$axis_h[[1]])
    )
    
    layout <- grid.layout(
      ncol = length(widths), widths = widths,
      nrow = length(heights), heights = heights,
      respect = respect
    )
    layout_vp <- viewport(layout=layout, name="panels")
    
    strip_rows <- nrow(guides$strip_h)
    panel_rows <- nrow(guides$panel)
    panel_cols <- ncol(guides$panel)
    
    children_vp <- do.call("vpList", c(
      setup_viewports("strip_h", guides$strip_h, c(0,1)),
      
      setup_viewports("axis_v",  guides$axis_v,  c(strip_rows, 0), "off"),
      setup_viewports("panel",   guides$panel,   c(strip_rows, 1)),
      setup_viewports("strip_v", guides$strip_v, c(strip_rows, 1 + panel_cols)),
      
      setup_viewports("axis_h",  guides$axis_h, c(strip_rows + panel_rows, 1), "off")
    ))
    
    vpTree(layout_vp, children_vp)
  }

  # Initialisation
  
  initialise <- function(., data) {
    .$shape <- stamp(data[[1]], .$facets, function(x) 0, margins=.$margins)
  }
  
  grid <- function(., data) .$shape
  
  # Position scales ----------------------------------------------------------
  
  position_train <- function(., data, scales) {
    if (is.null(.$scales_x)) {
      fr <- .$free
      .$scales_x <- scales_list(scales$get_scales("x"), ncol(.$shape), fr$x)
      .$scales_y <- scales_list(scales$get_scales("y"), nrow(.$shape), fr$y)
    }

    lapply(data, function(l) {
      for(i in seq_along(.$scales_x)) {
        lapply(l[, i], .$scales_x[[i]]$train_df)
      }
      for(i in seq_along(.$scales_y)) {
        lapply(l[i, ], .$scales_y[[i]]$train_df)
      }
    })
  }
  
  position_map <- function(., data, scales) {
    lapply(data, function(l) {
      for(i in seq_along(.$scales_x)) {
        l[, i] <- lapply(l[, i], function(old) {
          new <- .$scales_x[[i]]$map_df(old)
          cbind(new, old[setdiff(names(old), names(new))])
        }) 
      }
      for(i in seq_along(.$scales_y)) {
        l[i, ] <- lapply(l[i, ], function(old) {
          new <- .$scales_y[[i]]$map_df(old)
          cbind(new, old[setdiff(names(old), names(new))])
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

      for(i in seq_len(nrow(layerd))) {
        for(j in seq_len(ncol(layerd))) {
          s <- Scales$new()
          s$add(.$scales_y[[i]])
          s$add(.$scales_x[[j]])
          cs$train(s)
          
          grobs[[i, j]] <- layer$make_grob(layerd[[i, j]], scales, cs)
        }
      }
      grobs
    })
  }
  

  # Documentation ------------------------------------------------------------

  objname <- "grid"
  desc <- "Lay out panels in a rectangular/tabular manner."
  
  desc_params <- list(
    facets = "a formula with the rows (of the tabular display) on the LHS and the columns (of the tabular display) on the RHS; the dot in the formula is used to indicate there should be no faceting on this dimension (either row or column); the formula can also be entered as a string instead of a classical formula object",
    margins = "logical value, should marginal rows and columns be displayed"
  )
    
  seealso <- list(
    "cast" = "the formula and margin arguments are the same as those used in the reshape package"
  )  
  
  icon <- function(.) {
    gTree(children = gList(
      rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
    ))
  }  
  
  examples <- function(.) {
    # faceting displays subsets of the data in different panels
    p <- ggplot(diamonds, aes(x=carat, y=..density..)) + geom_histogram(binwidth=0.2)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate faceting specifications
    # p + facet_grid("cut ~ .")
    
    # see also ?plotmatrix for the scatterplot matrix
    
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", .$facets, ", ", .$margins, ")", sep="")
    if (newline) cat("\n")
  }
  
})

scales_list <- function(scale, n, free) {
  if (free) {
    rlply(n, scale$clone())  
  } else {
    rep(list(scale), n)  
  }
}
