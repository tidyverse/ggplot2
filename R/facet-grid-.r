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
      scales = NULL
    )
  }
  
  conditionals <- function(.) {
    vars <- all.vars(as.formula(.$facets))
    setdiff(vars, c(".", "..."))
  }
  
  stamp_data <- function(., data) {
    data <- add_missing_levels(data, .$conditionals())
    data <- lapply(data, function(df) {
      df <- stamp(add_group(df), .$facets, force, 
        margins=.$margins, fill = list(NULL), add.missing = TRUE)
      force_matrix(df)
    })
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels_grob, coord, theme) {
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio)) aspect_ratio <- 1

    nr <- nrow(panels_grob)
    nc <- ncol(panels_grob)
    
    coord_details <- matrix(list(), nrow = nr, ncol = nc)
    for (i in seq_len(nr)) {
      for(j in seq_len(nc)) {
        scales <- list(
          x = .$scales$x[[j]]$clone(), 
          y = .$scales$y[[i]]$clone()
        )        
        coord_details[[i, j]] <- coord$compute_ranges(scales)
      }
    }
    
    axes_h <- matrix(list(), nrow = 1, ncol = nc)
    for(i in seq_along(.$scales$x)) {
      axes_h[[1, i]] <- coord$guide_axis_h(coord_details[[1, i]], theme)
    }
    
    axes_v <- matrix(list(), nrow = nr, ncol = 1)
    for(i in seq_along(.$scales$y)) {
      axes_v[[i, 1]] <- coord$guide_axis_v(coord_details[[i, 1]], theme)
    }    
    
    labels <- .$labels_default(.$shape, theme)

    # Add background and foreground to panels
    panels <- matrix(list(), nrow=nr, ncol = nc)
    for(i in seq_len(nr)) {
      for(j in seq_len(nc)) {
        fg <- coord$guide_foreground(coord_details[[i, j]], theme)
        bg <- coord$guide_background(coord_details[[i, j]], theme)

        name <- paste("panel", i, j, sep = "_")
        panels[[i,j]] <- ggname(name, grobTree(bg, panels_grob[[i, j]], fg))
      }
    }
    
    # Add gaps and compute widths and heights

    gap <- matrix(list(nullGrob()), ncol = nc, nrow = nr)
    panels <- cweave(
      rweave(panels, gap),
      rweave(gap,    gap)
    )
    panels <- panels[-nrow(panels), -ncol(panels), drop = FALSE]
    
    axes_v <- rweave(axes_v, gap[, 1, drop = FALSE])
    strip_v <- rweave(labels$v, gap[, rep(1, ncol(labels$v)), drop = FALSE])

    axes_h <- cweave(axes_h, gap[1, , drop = FALSE])
    strip_h <- cweave(labels$h, gap[rep(1, nrow(labels$h)), , drop = FALSE])
    
    if(.$space_is_free) {
      size <- function(y) unit(diff(y$output_expand()), "null")
      panel_widths <- llply(.$scales$x, size)
      panel_heights <- llply(.$scales$y, size)
    } else {
      panel_widths <- rep(list(unit(1, "null")), nc)
      panel_heights <- rep(list(unit(1 * aspect_ratio, "null")), nr)
    }
    margin <- list(theme$panel.margin)
    panel_widths <- do.call("unit.c", interleave(panel_widths, margin))
    panel_widths[length(panel_widths)] <- unit(0, "cm")
    
    panel_heights <- do.call("unit.c", interleave(panel_heights, margin))
    panel_heights[length(panel_heights)] <- unit(0, "cm")

    list(
      panel     = panels, 
      axis_v    = axes_v,
      strip_v   = strip_v,
      axis_h    = axes_h,
      strip_h   = strip_h,
      widths    = panel_widths,
      heights   = panel_heights
    )
  }
  
  create_viewports <- function(., guides, theme) {
    aspect_ratio <- theme$aspect.ratio
    respect <- !is.null(aspect_ratio)
    if (is.null(aspect_ratio)) aspect_ratio <- 1
    
    strip_widths <- llply(guides$strip_v, grobWidth)
    strip_widths <- llply(1:ncol(strip_widths), function(i) 
      do.call("max", strip_widths[, i]))
    
    widths <- unit.c(
      do.call("max", llply(guides$axis_v, grobWidth)),
      guides$widths,
      do.call("unit.c", strip_widths)
    )
    
    strip_heights <- llply(guides$strip_h, grobHeight)
    strip_heights <- llply(1:nrow(strip_heights), function(i) 
      do.call("max", strip_heights[i, ]))
    
    heights <- unit.c(
      do.call("unit.c", strip_heights),
      guides$heights,
      do.call("max", llply(guides$axis_h, grobHeight))
    )
    
    layout <- grid.layout(
      ncol = length(widths), widths = widths,
      nrow = length(heights), heights = heights,
      respect = respect
    )
    layout_vp <- viewport(layout=layout, name="panels")
    
    strip_rows <- nrow(guides$strip_h)
    panel_rows <- nrow(guides$panel) + 1
    panel_cols <- ncol(guides$panel) + 1
    
    children_vp <- do.call("vpList", c(
      setup_viewports("strip_h", guides$strip_h, c(0,1)),
      
      setup_viewports("axis_v",  guides$axis_v,  c(strip_rows, 0), "off"),
      setup_viewports("panel",   guides$panel,   c(strip_rows, 1)),
      setup_viewports("strip_v", guides$strip_v, c(strip_rows, 1 + panel_cols)),
      
      setup_viewports("axis_h",  guides$axis_h, c(strip_rows + panel_rows, 1), "off")
    ))
    
    vpTree(layout_vp, children_vp)
  }

  labels_default <- function(., gm, theme) {
    add.names <- function(x) {
      for(i in 1:ncol(x)) x[[i]] <- theme$strip.label(colnames(x)[i], x[,i])
      x
    }

    row.labels <- add.names(rrownames(gm))
    col.labels <- add.names(rcolnames(gm))

    strip_h <- apply(col.labels, c(2,1), ggstrip, theme = theme)
    if (nrow(strip_h) == 1 && ncol(strip_h) == 1) strip_h <- matrix(list(nullGrob()))
    strip_v <- apply(row.labels, c(1,2), ggstrip, horizontal=FALSE, theme=theme)
    if (nrow(strip_v) == 1 && ncol(strip_v) == 1) strip_v <- matrix(list(nullGrob()))

    list(
      h = strip_h, 
      v = strip_v
    )
  }

  # Initialisation
  
  initialise <- function(., data) {
    .$shape <- stamp(data[[1]], .$facets, function(x) 0, margins=.$margins)
  }
  
  # Position scales ----------------------------------------------------------
  
  position_train <- function(., data, scales) {
    if (is.null(.$scales$x) && scales$has_scale("x")) {
      .$scales$x <- scales_list(
        scales$get_scales("x"), ncol(.$shape), .$free$x)
    }
    if (is.null(.$scales$y) && scales$has_scale("y")) {
      .$scales$y <- scales_list(
        scales$get_scales("y"), nrow(.$shape), .$free$y)
    }
    
    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        lapply(l[, i], .$scales$x[[i]]$train_df, drop = .$free$x)
      }
      for(i in seq_along(.$scales$y)) {
        lapply(l[i, ], .$scales$y[[i]]$train_df, drop = .$free$y)
      }
    })
  }
  
  position_map <- function(., data, scales) {
    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        l[, i] <- lapply(l[, i], function(old) {
          if (is.null(old)) return(data.frame())
          new <- .$scales$x[[i]]$map_df(old)
          cbind(new, old[setdiff(names(old), names(new))])
        }) 
      }
      for(i in seq_along(.$scales$y)) {
        l[i, ] <- lapply(l[i, ], function(old) {
          if (is.null(old)) return(data.frame())
          new <- .$scales$y[[i]]$map_df(old)
          cbind(new, old[setdiff(names(old), names(new))])
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

      for(i in seq_len(nrow(layerd))) {
        for(j in seq_len(ncol(layerd))) {
          scales <- list(
            x = .$scales$x[[j]]$clone(), 
            y = .$scales$y[[i]]$clone()
          )
          details <- coord$compute_ranges(scales)
          grobs[[i, j]] <- layer$make_grob(layerd[[i, j]], details, coord)
        }
      }
      grobs
    })
  }
  
  calc_statistics <- function(., data, layers) {
    lapply(seq_along(data), function(i) {
      layer <- layers[[i]]
      layerd <- data[[i]]
      grobs <- matrix(list(), nrow = nrow(layerd), ncol = ncol(layerd))

      for(i in seq_len(nrow(layerd))) {
        for(j in seq_len(ncol(layerd))) {
          scales <- list(
            x = .$scales$x[[j]], 
            y = .$scales$y[[i]]
          )
          grobs[[i, j]] <- layer$calc_statistic(layerd[[i, j]], scales)
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
    # "cast" = "the formula and margin arguments are the same as those used in the reshape package"
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
    p <- ggplot(diamonds, aes(carat, ..density..)) +
     geom_histogram(binwidth = 0.2)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    
    qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
    qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate faceting specifications
    # p + facet_grid("cut ~ .")
    
    # see also ?plotmatrix for the scatterplot matrix
    
    # If there isn't any data for a given combination, that panel 
    # will be empty
    qplot(mpg, wt, data=mtcars) + facet_grid(cyl ~ vs)
    
    # If you combine a facetted dataset with a dataset that lacks those
    # facetting variables, the data will be repeated across the missing
    # combinations:
    p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)

    df <- data.frame(mpg = 22, wt = 3)
    p + geom_point(data = df, colour="red", size = 2)
    
    df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
    p + geom_point(data = df2, colour="red", size = 2)

    df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
    p + geom_point(data = df2, colour="red", size = 2)

    
    # You can also choose whether the scales should be constant
    # across all panels (the default), or whether they should be allowed
    # to vary
    mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
    
    mt + facet_grid(. ~ cyl, scales = "free")
    # If scales and space are free, then the mapping between position
    # and values in the data will be the same across all panels
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

# List of scales
# Make a list of scales, cloning if necessary
# 
# @arguments input scale
# @arguments number of scales to produce in output
# @arguments should the scales be free (TRUE) or fixed (FALSE)
# @keywords internal
scales_list <- function(scale, n, free) {
  if (free) {
    rlply(n, scale$clone())  
  } else {
    rep(list(scale), n)  
  }
}
