label_value <- function(variable, value) value
label_both <- function(variable, value) paste(variable, value, sep = ": ")

#X mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#X qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
#X qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2, 
#X   labeller = label_parsed)
label_parsed <- function(variable, value) {
  llply(as.character(value), function(x) parse(text = x))
}

#X p <- qplot(wt, mpg, data = mtcars)
#X p + facet_grid(~ vs + am, labeller = label_bquote(alpha ^ .(x)))
#X p + facet_grid(~ vs + am, labeller = label_bquote(.(x) ^ .(x)))
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