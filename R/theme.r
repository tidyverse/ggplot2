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

get_plot_theme <- function(x) {
  defaults(x$options, default_theme)
}
