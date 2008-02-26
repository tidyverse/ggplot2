# Try, with default in case of error
# \code{try_default} wraps try so that it returns a default value in the case of error.
# 
# \code{tryNULL} provides a useful special case when dealing with lists.
# 
# @alias tryNULL
# @arguments expression to try
# @arguments default value in case of error
# @keyword internal
# @seealso \code{\link{tryapply}}
try_default <- function(expr, default = NA) {
  result <- default
  tryCatch(result <- expr, error = function(e) {})
  result
}
tryNULL <- function(expr) try_default(expr, NULL)

# Apply with built in try
# Uses compact, lapply and tryNULL
# 
# @keyword internal
tryapply <- function(list, fun, ...) {
  compact(lapply(list, function(x) tryNULL(fun(x, ...))))
}
