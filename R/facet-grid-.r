FacetGrid <- proto(Facet, {
  new <- function(., facets = . ~ ., margins = FALSE, scales = "fixed", space = "fixed", labeller = "label_value", as.table = TRUE, widths = NULL, heights = NULL) {
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
      scales = NULL, labeller = list(labeller), as.table = as.table,
      space_widths = widths, space_heights = heights
    )
  }
  
  conditionals <- function(.) {
    vars <- all.vars(as.formula(.$facets))
    setdiff(vars, c(".", "..."))
  }
  
  
  # Initialisation  
  initialise <- function(., data) {
    .$facet_levels <- unique(
      ldply(data, failwith(NULL, "[", quiet = TRUE), .$conditionals()))
    
    .$shape <- stamp(.$facet_levels, .$facets, margins = .$margins,
      function(x) 0)
  }

  
  stamp_data <- function(., data) {
    data <- add_missing_levels(data, .$facet_levels)
    data <- lapply(data, function(df) {
      if (empty(df)) return(force_matrix(data.frame()))
      df <- stamp(add_group(df), .$facets, force, 
        margins=.$margins, fill = list(data.frame()), add.missing = TRUE)
      force_matrix(df)
    })
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels_grob, coord, theme) {

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

    nr <- nrow(panels_grob)
    nc <- ncol(panels_grob)
    
    coord_details <- matrix(list(), nrow = nr, ncol = nc)
    for (i in seq_len(nr)) {
      for(j in seq_len(nc)) {
        scales <- list(
          x = .$scales$x[[j]], 
          y = .$scales$y[[i]]
        )        
        coord_details[[i, j]] <- coord$compute_ranges(scales)
      }
    }
    
    # Horizontal axes
    axes_h <- list()
    for(i in seq_along(.$scales$x)) {
      axes_h[[i]] <- coord$guide_axis_h(coord_details[[1, i]], theme)
    }
    axes_h_height <- do.call("max2", llply(axes_h, grobHeight))
    axeshGrid <- grobGrid(
      "axis_h", axes_h, nrow = 1, ncol = nc,
      heights = axes_h_height, clip = "off"
    )
    
    
    # Vertical axes
    axes_v <- list()
    for(i in seq_along(.$scales$y)) {
      axes_v[[i]] <- coord$guide_axis_v(coord_details[[i, 1]], theme)
    }    
    axes_v_width <- do.call("max2", llply(axes_v, grobWidth))
    axesvGrid <- grobGrid(
      "axis_v", axes_v, nrow = nr, ncol = 1,
      widths = axes_v_width, as.table = .$as.table, clip = "off"
    )
    
    # Strips
    labels <- .$labels_default(.$shape, theme)
    
    strip_widths <- llply(labels$v, grobWidth)
    strip_widths <- do.call("unit.c", llply(1:ncol(strip_widths), 
      function(i) do.call("max2", strip_widths[, i])))
    stripvGrid <- grobGrid(
      "strip_v", t(labels$v), nrow = nrow(labels$v), ncol = ncol(labels$v),
      widths = strip_widths, as.table = .$as.table
    )

    strip_heights <- llply(labels$h, grobHeight)
    strip_heights <- do.call("unit.c", llply(1:nrow(strip_heights),
       function(i) do.call("max2", strip_heights[i, ])))
    striphGrid <- grobGrid(
      "strip_h", t(labels$h), nrow = nrow(labels$h), ncol = ncol(labels$h),
      heights = strip_heights
    )
      
    # Add background and foreground to panels
    panels <- matrix(list(), nrow=nr, ncol = nc)
    for(i in seq_len(nr)) {
      for(j in seq_len(nc)) {
        fg <- coord$guide_foreground(coord_details[[i, j]], theme)
        bg <- coord$guide_background(coord_details[[i, j]], theme)

        panels[[i,j]] <- grobTree(bg, panels_grob[[i, j]], fg)
      }
    }

    if(.$space_is_free) {
      size <- function(y) unit(diff(y$output_expand()), "null")
      panel_widths <- do.call("unit.c", llply(.$scales$x, size))
      panel_heights <- do.call("unit.c", llply(.$scales$y, size))
    } else {
      if (!is.null(.$space_widths)) {
        panel_widths <- do.call("unit.c", lapply(.$space_widths, function(x)unit(x, "null")))
      } else {
        panel_widths <- unit(1, "null")
      }
      if (!is.null(.$space_heights)) {
        panel_heights <- do.call("unit.c", lapply(.$space_heights, function(x)unit(x, "null")))
      } else {
        panel_heights <- unit(1 * aspect_ratio, "null")
      }
    }
    

    panelGrid <- grobGrid(
      "panel", t(panels), ncol = nc, nrow = nr,
      widths = panel_widths, heights = panel_heights, as.table = .$as.table,
      respect = respect
    )
       
    # Add gaps and compute widths and heights
    fill_tl <- spacer(nrow(labels$h), 1)
    fill_tr <- spacer(nrow(labels$h), ncol(labels$v))
    fill_bl <- spacer(1, 1)
    fill_br <- spacer(1, ncol(labels$v))
    
    all <- rbind(
      cbind(fill_tl,   striphGrid, fill_tr),
      cbind(axesvGrid, panelGrid,  stripvGrid),
      cbind(fill_bl,   axeshGrid,  fill_br) 
    )
    # theme$panel.margin, theme$panel.margin
    
    # from left to right
    hgap_widths <- do.call("unit.c", compact(list(
      unit(0, "cm"), # no gap after axis
      rep.unit2(theme$panel.margin, nc - 1), # gap after all panels except last
      unit(rep(0, ncol(stripvGrid) + 1), "cm") # no gap after strips 
    )))
    hgap <- grobGrid("hgap", 
      ncol = ncol(all), nrow = nrow(all),
      widths = hgap_widths, 
    )
    
    # from top to bottom
    vgap_heights <- do.call("unit.c", compact(list(
      rep(unit(0, "cm"), 2), # no gap before and after axis
      rep.unit2(theme$panel.margin, nr - 1), # gap after all panels except last
      unit(rep(0, nrow(striphGrid)), "cm") # no gap after strips
    )))
    
    vgap <- grobGrid("vgap",
      nrow = nrow(all), ncol = ncol(all) * 2,
      heights = vgap_heights
    )
    
    rweave(cweave(all, hgap), vgap)
  }


  labels_default <- function(., gm, theme) {
    labeller <- match.fun(.$labeller[[1]])
    add.names <- function(x) {
      for(i in 1:ncol(x)) x[[i]] <- labeller(colnames(x)[i], x[,i])
      x
    }

    row.labels <- add.names(rrownames(gm))
    col.labels <- add.names(rcolnames(gm))

    strip_h <- apply(col.labels, c(2,1), ggstrip, theme = theme)
    if (nrow(strip_h) == 1 && ncol(strip_h) == 1) strip_h <- matrix(list(zeroGrob()))
    strip_v <- apply(row.labels, c(1,2), ggstrip, horizontal=FALSE, theme=theme)
    if (nrow(strip_v) == 1 && ncol(strip_v) == 1) strip_v <- matrix(list(zeroGrob()))

    list(
      h = strip_h, 
      v = strip_v
    )
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
        lapply(l[, i], scale_train_df, scale = .$scales$x[[i]])
      }
      for(i in seq_along(.$scales$y)) {
        lapply(l[i, ], scale_train_df, scale = .$scales$y[[i]])
      }
    })
  }
  
  position_map <- function(., data, scales) {
    lapply(data, function(l) {
      for(i in seq_along(.$scales$x)) {
        l[, i] <- lapply(l[, i], function(old) {
          if (is.null(old)) return(data.frame())
          new <- scale_map_df(.$scales$x[[i]], old)
          if (length(new) == 0) return(old)
          cbind(new, old[setdiff(names(old), names(new))])
        }) 
      }
      for(i in seq_along(.$scales$y)) {
        l[i, ] <- lapply(l[i, ], function(old) {
          if (is.null(old)) return(data.frame())
          new <- scale_map_df(.$scales$y[[i]], old)
          if (length(new) == 0) return(old)
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
            x = .$scales$x[[j]], 
            y = .$scales$y[[i]]
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
     geom_histogram(binwidth = 1)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    # p + facet_grid(cut ~ clarity, margins=TRUE)
    
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
    p <- qplot(mpg, wt, data=mtcars, facets = vs ~ cyl)

    df <- data.frame(mpg = 22, wt = 3)
    p + geom_point(data = df, colour="red", size = 2)
    
    df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
    p + geom_point(data = df2, colour="red", size = 2)

    df3 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
    p + geom_point(data = df3, colour="red", size = 2)

    
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
# @keyword internal
scales_list <- function(scale, n, free) {
  if (free) {
    rlply(n, scale_clone(scale))  
  } else {
    rep(list(scale), n)  
  }
}
