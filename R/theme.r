# Get, set and update themes.
# These three functions get, set and update themes.
# 
# Use \code{theme_update} to modify a small number of elements of the current
# theme or use \code{theme_set} to completely override it.
# 
# @alias theme_set
# @alias theme_get
theme_update <- function(...) {
  elements <- list(...)
  if (length(args) == 1 && is.list(elements[[1]])) {
    elements <- elements[[1]]
  }
  
  theme_set(defaults(elements, theme_get()))  
}

.theme <- (function() {
  theme <- theme_gray()

  list(
    get = function() theme,
    set = function(new) {
      old <- theme
      theme <<- new
      invisible(old)
    }
  )
})()
theme_get <- .theme$get  
theme_set <- .theme$set

# Plot options
# Set options/theme elements for a single plot
# 
# @argument named list of theme settings
opts <- function(...) {
  structure(list(...), class="options")
}

# Render a theme element
# This function is used internally for all drawing of plot surrounds etc
# 
# It also names the created grobs consistently
# 
# @keywords internal
theme_render <- function(theme, element, ..., name = NULL) {
  el <- theme[[element]]
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(nullGrob())
  }
  
  ggname(ps(element, name, sep = "."), el(...))
}

# Print out a theme element
# Currently all theme elements save there call, which is printed here
# 
# @keywords internal
print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

# Retrieve theme for a plot
# Combines plot defaults with current theme to get complete theme for a plot
# 
# @arugments plot
# @keywords internal
plot_theme <- function(x) {
  defaults(x$options, theme_get())
}
