Facet <- proto(TopLevel, {
  objname <- "Facet"
  class <- function(.) "facet"
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a facet object.</p>"
    )
  }
  
  call <- function(.) {
    ps(
      .$my_name(), "(",
      "\n<br />&nbsp;&nbsp;",
      ps(
        plist(.$parameters())
      ), 
      "\n<br />)", collapse="\n<br />"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
})