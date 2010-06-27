Coord <- proto(TopLevel, expr={
  limits <- list()
  class <- function(.) "coord"
  
  muncher <- function(.) FALSE
  
  # Rescaling at coord level should not be clipped: this is what 
  # makes zooming work
  rescale_var <- function(., data, range, clip = FALSE) {
    rescale(data, 0:1, range, clip = clip)
  }
  
  munch <- function(., data, details, segment_length = 0.02) {
    if (!.$muncher()) return(.$transform(data, details))
    
    # Transform to calculate distances
    trans <- .$transform(data[c("x", "y", "group")], details)
    dist <- compute_distance(trans$x, trans$y, trans$group)
    
    # Munch and then transform result
    munched <- munch_data(data, dist, segment_length)
    .$transform(munched, details)
  }
    
  compute_aspect <- function(., ranges) {
    NULL
  }
  
  labels <- function(., scales) {
    scales
  }
  
  pprint <- function(., newline=TRUE) {
    args <- formals(get("new", .))
    args <- args[!names(args) %in% c(".", "...")]
  
    cat("coord_", .$objname, ": ", clist(args), sep="")
    
    if (newline) cat("\n") 
  }
  
  guide_foreground <- function(., scales, theme) {
    theme_render(theme, "panel.border")
  }  
  # Html defaults
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a coordinate system object.</p>"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c("."))]
  }
  
})
