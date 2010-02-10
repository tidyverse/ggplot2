# Get, set and update themes.
# These three functions get, set and update themes.
# 
# Use \code{theme_update} to modify a small number of elements of the current
# theme or use \code{theme_set} to completely override it.
# 
# @alias theme settings to override
# @alias theme_set
# @alias theme_get
# @alias ggopt
# @arguments named list of theme settings
#X qplot(mpg, wt, data = mtcars)
#X old <- theme_set(theme_bw())
#X qplot(mpg, wt, data = mtcars)
#X theme_set(old)
#X qplot(mpg, wt, data = mtcars)
#X
#X old <- theme_update(panel.background = theme_rect(colour = "pink"))
#X qplot(mpg, wt, data = mtcars)
#X theme_set(old)
#X theme_get()
#X 
#X qplot(mpg, wt, data=mtcars, colour=mpg) + 
#X   opts(legend.position=c(0.95, 0.95), legend.justification = c(1, 1))
#X last_plot() + 
#X  opts(legend.background = theme_rect(fill = "white", col="white", size =3))
theme_update <- function(...) {
  elements <- list(...)
  if (length(args) == 1 && is.list(elements[[1]])) {
    elements <- elements[[1]]
  }
  theme <- plyr::defaults(elements, theme_get())
  class(theme) <- c("options")
  
  theme_set(theme)  
}

.theme <- (function() {
  theme <- theme_gray()

  list(
    get = function() theme,
    set = function(new) {
      missing <- setdiff(names(theme_gray()), names(new))
      if (length(missing) > 0) {
        warning("New theme missing the following elements: ", 
          paste(missing, collapse = ", "), call. = FALSE)
      }
      
      old <- theme
      theme <<- new
      invisible(old)
    }
  )
})()
theme_get <- .theme$get  
theme_set <- .theme$set

ggopt <- function(...) {
  .Deprecated("theme_update")
}

# Plot options
# Set options/theme elements for a single plot
# 
# Use this function if you want to modify a few theme settings for 
# a single plot.
# 
# @arguments named list of theme settings
#X p <- qplot(mpg, wt, data = mtcars)
#X p 
#X p + opts(panel_background = theme_rect(colour = "pink"))
#X p + theme_bw()
opts <- function(...) {
  structure(list(...), class="options")
}

# Render a theme element
# This function is used internally for all drawing of plot surrounds etc
# 
# It also names the created grobs consistently
# 
# @keyword internal
theme_render <- function(theme, element, ..., name = NULL) {
  el <- theme[[element]]
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }
  
  ggname(ps(element, name, sep = "."), el(...))
}

# Print out a theme element
# Currently all theme elements save there call, which is printed here
# 
# @keyword internal
print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

# Retrieve theme for a plot
# Combines plot defaults with current theme to get complete theme for a plot
# 
# @arguments plot
# @keyword internal
plot_theme <- function(x) {
  plyr::defaults(x$options, theme_get())
}
