print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

theme_render <- function(theme, name, ...) {
  el <- theme[[name]]
  if (is.null(el)) {
    message("Theme element ", name, " missing")
    nullGrob()
  }

  ggname(name, el(...))
}

get_plot_theme <- function(x) {
  defaults(x$options, default_theme)
}
