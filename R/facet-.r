Facet <- proto(TopLevel, {
  clone <- function(.) {
    as.proto(.$as.list(all.names=TRUE), parent=.) 
  }
  objname <- "Facet"
  class <- function(.) "facet"
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a facet object.</p>"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
  xlabel <- function(., theme) 
    theme_render(theme, "axis.title.x", .$scales$x[[1]]$name)
  ylabel <- function(., theme) 
    theme_render(theme, "axis.title.y", .$scales$y[[1]]$name)
})