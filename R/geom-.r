Geom <- proto(TopLevel, expr={
  class <- function(.) "geom"

  parameters <- function(.) {
    params <- formals(get("draw", .))
    params <- params[setdiff(names(params), c(".","data","scales", "coordinates", "..."))]
    
    required <- rep(NA, length(.$required_aes))
    names(required) <- .$required_aes
    aesthetics <- c(.$default_aes(), required)
    
    c(params, aesthetics[setdiff(names(aesthetics), names(params))])
  }
  
  required_aes <- c()
  default_aes <- function(.) {}
  default_pos <- function(.) PositionIdentity

  draw <- function(...) {}
  draw_groups <- function(., data, scales, coordinates, ...) {
    if (is.null(data) || nrow(data) == 0) return()
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) .$draw(group, scales, coordinates, ...))
    
    ggname(paste(.$objname, "s", sep=""), gTree(
      children = do.call("gList", grobs)
    ))
  }
  
  adjust_scales_data <- function(., scales, data) data
  
  new <- function(., mapping=NULL, data=NULL, stat=NULL, position=NULL, ...){
    do.call("layer", list(mapping=mapping, data=data, stat=stat, geom=., position=position, ...))
  }
  
  pprint <- function(., newline=TRUE) {
    cat("geom_", .$objname, ": ", clist(.$parameters()), sep="")
    if (newline) cat("\n")
  }
  
  # Html documentation ----------------------------------
  
  
    
  
})