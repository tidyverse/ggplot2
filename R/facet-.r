Facet <- proto(TopLevel, {
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
})