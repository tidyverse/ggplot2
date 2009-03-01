label_value <- function(variable, value) value
label_both <- function(variable, value) paste(variable, value, sep = ": ")
label_parsed <- function(variable, value) parse(text = value)

label_bquote <- function(expr = beta ^ .(x)) {
  quoted <- substitute(expr)
  
  function(variable, value) 
    lapply(value, function(x)
      eval(substitute(bquote(expr, list(x = x)), list(expr = quoted))))
}

# Grob strip
# Grob for strip labels
# 
# @arguments text to display
# @arguments orientation, horizontal or vertical
# @keyword hplot 
# @keyword internal
ggstrip <- function(text, horizontal=TRUE, theme) {
  text_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  label <- theme_render(theme, text_theme, text)

  ggname("strip", absoluteGrob(
    gList(
      theme_render(theme, "strip.background"),
      label
    ),
    width = grobWidth(label) + unit(0.5, "lines"),
    height = grobHeight(label) + unit(0.5, "lines")
  ))
}


# Return list of strip grobs
strips <- function(labels, theme, direction = "vertical") {
  
}