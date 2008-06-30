print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

draw_element <- function(theme, name, ...) {
  el <- theme[[name]]
  ggname(name, el(...))
}
draw_element(default_theme, "axis.box")
