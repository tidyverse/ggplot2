Coord <- proto(TopLevel, expr={
  limits <- list()
  class <- function(.) "coord"
  
  muncher <- function(.) FALSE
  
  # Rescaling at coord level should not be clipped: this is what 
  # makes zooming work
  rescale_var <- function(., data, range, clip = FALSE) {
    rescale(data, 0:1, range, clip = clip)
  }
  
  munch <- function(., data, details, segment_length = 0.01) {
    if (!.$muncher()) return(.$transform(data, details))
    
    # Calculate distances using coord distance metric
    dist <- .$distance(data$x, data$y, details)
    dist[data$group[-1] != data$group[-nrow(data)]] <- NA
    
    # Munch and then transform result
    munched <- munch_data(data, dist, segment_length)
    .$transform(munched, details)
  }
  
  distance <- function(., x, y, details) {
    max_dist <- dist_euclidean(details$x.range, details$y.range)    
    dist_euclidean(x, y) / max_dist
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
