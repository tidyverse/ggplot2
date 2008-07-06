# Position adjustment occurs over all groups within a geom
# They work only with discrete x scales and may affect x and y position.
# Should occur after statistics and scales have been applied.

Position <- proto(TopLevel, expr = {
  adjust <- function(., data, scales, ...) data

  class <- function(.) "position"
  
  new <- function(.) .$proto()

  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c("."))]
  }
  
  pprint <- function(., newline=TRUE) {
    cat("position_", .$objname, ": ()", sep="")
    if (newline) cat("\n")
  }

  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a position object.</p>"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
})


# Convenience function to ensure that all position variables 
# (x, xmin, xmax, xend) are transformed in the same way
transform_position <- function(df, trans_x, trans_y) {
  xs <- grep("^x", names(df))
  ys <- grep("^y", names(df))
  
  df[xs] <- lapply(df[xs], trans_x)
  df[ys] <- lapply(df[ys], trans_y)
  df
}