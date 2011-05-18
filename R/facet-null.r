FacetNull <- proto(Facet, {
  new <- function(.) {    
    .$proto()
  }

  # The null facetter has a single panel.
  panel_info <- function(., data) {     
    data.frame(
      PANEL = 1, ROW = 1, COL = 1, 
      SCALE_X = 1, SCALE_Y = 1)
  }

  map_layer <- function(., data) {
    transform(data, PANEL = 1)
  }

  # Create grobs for each component of the panel guides
  add_guides <- function(., panels_grob, coord, theme) {

    aspect_ratio <- theme$aspect.ratio
    coord_details <- coord$compute_ranges(.$panel_scales(1))
    
    # If user hasn't set aspect ratio ask the coordinate system if it 
    # wants to specify one
    if (is.null(aspect_ratio)) {
      aspect_ratio <- coord$compute_aspect(coord_details)
    }
    
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    
    axis_h <- coord$guide_axis_h(coord_details, theme)
    axis_v <- coord$guide_axis_v(coord_details, theme)

    fg <- coord$guide_foreground(coord_details, theme)
    bg <- coord$guide_background(coord_details, theme)
    panel_grob <- grobTree(bg, panels_grob[[1]], fg)      
    
    all <- matrix(list(
      axis_v,     panel_grob,
      zeroGrob(), axis_h
    ), ncol = 2, byrow = T)
    
    layout_matrix("layout", all, 
      widths = unit.c(grobWidth(axis_v), unit(1, "null")),
      heights = unit.c(unit(1, "null"), grobHeight(axis_h))
    )
  }


  # Documentation ------------------------------------------------------------

  objname <- "null"
  desc <- "A single panel"
    
  icon <- function(.) {
    gTree(children = gList(
      rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
    ))
  }  
  
  examples <- function(.) {
    # facet_null is the default facetting specification if you don't override
    # it with facet_grid or facet_wrap
    ggplot(mtcars, aes(mpg, wt)) + geom_point()
    qplot(mpg, wt, data = mtcars)
  }

})
