Facet <- proto(TopLevel, {
  clone <- function(.) {
    as.proto(.$as.list(all.names=TRUE), parent=.) 
  }
  objname <- "Facet"
  shrink <- TRUE
  class <- function(.) "facet"
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a facet object.</p>"
    )
  }
  
  position_reset <- function(., scales) {
    if (!.$shrink) return()
    lapply(.$scales$x, scale_reset)
    lapply(.$scales$y, scale_reset)
    invisible()
  }
  
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
  xlabel <- function(., theme) {
    nulldefault(.$scales$x[[1]]$name, theme$labels$x)
  }
    
  ylabel <- function(., theme) 
    nulldefault(.$scales$y[[1]]$name, theme$labels$y)
})