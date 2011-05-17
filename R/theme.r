#' Get, set and update themes.
#' 
#' Use \code{theme_update} to modify a small number of elements of the current
#' theme or use \code{theme_set} to completely override it.
#' 
#' @param ... named list of theme settings
#' @export
#' @examples
#' qplot(mpg, wt, data = mtcars)
#' old <- theme_set(theme_bw())
#' qplot(mpg, wt, data = mtcars)
#' theme_set(old)
#' qplot(mpg, wt, data = mtcars)
#'
#' old <- theme_update(panel.background = theme_rect(colour = "pink"))
#' qplot(mpg, wt, data = mtcars)
#' theme_set(old)
#' theme_get()
#' 
#' qplot(mpg, wt, data=mtcars, colour=mpg) + 
#'   opts(legend.position=c(0.95, 0.95), legend.justification = c(1, 1))
#' last_plot() + 
#'  opts(legend.background = theme_rect(fill = "white", col="white", size =3))
theme_update <- function(...) {
  elements <- list(...)
  if (length(args) == 1 && is.list(elements[[1]])) {
    elements <- elements[[1]]
  }
  theme <- defaults(elements, theme_get())
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

#' Set options/theme elements for a single plot
#' 
#' Use this function if you want to modify a few theme settings for 
#' a single plot.
#' 
#' @param ... named list of theme settings
#' @export
#' @examples
#' p <- qplot(mpg, wt, data = mtcars)
#' p 
#' p + opts(panel_background = theme_rect(colour = "pink"))
#' p + theme_bw()
opts <- function(...) {
  structure(list(...), class="options")
}

# Render a theme element
theme_render <- function(theme, element, ..., name = NULL) {
  el <- theme[[element]]
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }
  
  ggname(ps(element, name, sep = "."), el(...))
}

#' Print out a theme element
#'
#' @keywords internal
#' @S3method print theme
print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$options, theme_get())
}
