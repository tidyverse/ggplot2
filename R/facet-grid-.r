FacetGrid <- proto(Facet, {
  new <- function(., facets = . ~ ., margins = FALSE) {
    if (inherits(facets, "formula")) facets <- deparse(facets) 
    .$proto(facets=facets, margins=margins)
  }
  
  conditionals <- function(.) {
    vars <- all.vars(as.formula(.$facets))
    setdiff(vars, c(".", "..."))
  }
  
  stamp_data <- function(., data) {
    data.matrix <- stamp(add_group(data), .$facets, force, margins=.$margins, fill=list(NULL))
    
    force_matrix(data.matrix)
  }
  
  grid <- function(., data) {
    stamp(data, .$facets, function(x) 0, margins=.$margins)
  }
  
  # Create grobs for each component of the panel guides
  add_guides <- function(., data, panels, coordinates, theme) {
    guides <- coordinates$guide_axes()
    
    nr <- nrow(panels)
    nc <- ncol(panels)
    
    axes_v <- matrix(list(guides$y), nrow = nr, ncol = 1)
    axes_h <- matrix(list(guides$x), nrow = 1, ncol = nc)
    labels <- labels_default(.$grid(data), theme)

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
    aspect_ratio <- theme$aspect_ratio
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
      
      setup_viewports("axis_v",  guides$axis_v,  c(strip_rows, 0)),
      setup_viewports("panel",   guides$panel,   c(strip_rows, 1)),
      setup_viewports("strip_v", guides$strip_v, c(strip_rows, 1 + panel_cols)),
      
      setup_viewports("axis_h",  guides$axis_h, c(strip_rows + panel_rows, 1))
    ))
    
    vpTree(layout_vp, children_vp)
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
    # Facetting displays subsets of the data in different panels
    p <- ggplot(diamonds, aes(x=carat, y=..density..)) + geom_histogram(binwidth=0.2)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate facetting specifications
    # p + facet_grid("cut ~ .")
    
    # see also ?plotmatrix for the scatterplot matrix
    
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", .$facets, ", ", .$margins, ")", sep="")
    if (newline) cat("\n")
  }
  
})
