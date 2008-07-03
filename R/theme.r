print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

theme_render <- function(theme, element, ..., name = NULL) {
  el <- theme[[element]]
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(nullGrob())
  }
  
  ggname(ps(element, name, sep = "."), el(...))
}

plot_theme <- function(x) {
  defaults(x$options, theme_get())
}

.theme <- (function() {
  theme <- theme_gray()

  list(
    get = function() theme,
    set = function(new) theme <<- new
  )
})()
theme_get <- .theme$get  
theme_set <- .theme$set

theme_update <- function(...) {
  elements <- list(...)
  if (length(args) == 1 && is.list(elements[[1]])) {
    elements <- elements[[1]]
  }
  
  theme_set(defaults(elements, theme_get()))  
}

opts <- function(...) {
  structure(list(...), class="options")
}
