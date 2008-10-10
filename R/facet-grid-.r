FacetGrid <- proto(Facet, {
  new <- function(., facets = . ~ ., margins = FALSE, scales = "fixed", space = "fixed") {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
    space <- match.arg(space, c("fixed", "free"))
    
    if (is.formula(facets)) facets <- deparse(facets) 
    .$proto(
      facets = facets, margins = margins,
      free = free, space_is_free = (space == "free"),
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
  add_guides <- function(., data, panels_grob, coordinates, theme) {
    nr <- nrow(panels_grob)
    nc <- ncol(panels_grob)
    
    axes_h <- matrix(list(), nrow = 1, ncol = nc)
    for(i in seq_along(.$scales_x)) {
      axes_h[[1, i]] <- coordinates$guide_axes(.$scales_x[[i]], theme, "bottom")
    }
    
    axes_v <- matrix(list(), nrow = nr, ncol = 1)
    for(i in seq_along(.$scales_y)) {
      axes_v[[i, 1]] <- coordinates$guide_axes(.$scales_y[[i]], theme, "left")
    }    
    
    labels <- labels_default(.$shape, theme)

    # Add background and foreground to panels
    panels <- matrix(list(), nrow=nr, ncol = nc)
    for(i in seq_len(nr)) {
      for(j in seq_len(nc)) {
        scales <- list(
          x = .$scales_x[[j]], 
          y = .$scales_y[[i]]
        )
        fg <- coordinates$guide_foreground(scales, theme)
        bg <- coordinates$guide_background(scales, theme)

        panels[[i,j]] <- grobTree(bg, panels_grob[[i, j]], fg)
      }
    }
    bg_empty <- theme_render(theme, "panel.empty")
    
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
    
    if(.$space_is_free) {
      panel_widths <- unit(
        laply(.$scales_x, function(y) diff(y$output_expand())), 
        "null"
      )
      panel_heights <- unit(
        laply(.$scales_y, function(y) diff(y$output_expand())), 
        "null"
      )
    } else {
      panel_widths <- rep(unit(1, "null"), ncol(guides$panel))
      panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow(guides$panel))
    }
    
    widths <- unit.c(
      do.call("max", llply(guides$axis_v, grobWidth)),
      panel_widths,
      do.call("max", lapply(guides$strip_v, grobWidth))
    )
    
    heights <- unit.c(
      do.call("max", lapply(guides$strip_h, grobHeight)),
      panel_heights,
      do.call("max", llply(guides$axis_h, grobHeight))
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
          scales <- list(
            x = .$scales_x[[j]], 
            y = .$scales_y[[i]]
          )
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
    
    # Example of free scales
    mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
    
    mt + facet_grid(. ~ cyl, scales = "free")
    mt + facet_grid(. ~ cyl, scales = "free", space = "free")
    
    mt + facet_grid(vs ~ am, scales = "free")
    mt + facet_grid(vs ~ am, scales = "free_x")
    mt + facet_grid(vs ~ am, scales = "free_y")
    mt + facet_grid(vs ~ am, scales = "free", space="free")

    # You may need to set your own breaks for consitent display:
    mt + facet_grid(. ~ cyl, scales = "free_x", space="free") + 
      scale_x_continuous(breaks = seq(10, 36, by = 2))
    # Adding scale limits override free scales:
    last_plot() + xlim(10, 15)

    # Free scales are particularly useful for categorical variables
    qplot(cty, model, data=mpg) + 
      facet_grid(manufacturer ~ ., scales = "free", space = "free")
    # particularly when you reorder factor levels
    mpg <- within(mpg, {
      model <- reorder(model, cty)
      manufacturer <- reorder(manufacturer, cty)
    })
    last_plot() %+% mpg + opts(strip.text.y = theme_text())


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
