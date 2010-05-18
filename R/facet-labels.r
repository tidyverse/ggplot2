# Label facets with their value
# The default facet labelling just uses the value of the variable
# 
# @arguments variable name passed in by facetter
# @arguments variable value passed in by facetter
# @keyword hplot
#X p <- qplot(wt, mpg, data = mtcars)
#X p + facet_grid(~ cyl)
#X p + facet_grid(~ cyl, labeller = label_value)
label_value <- function(variable, value) value

# Label facets with value and variable
# Join together facet value and the name of the variable to create a label.
# 
# @arguments variable name passed in by facetter
# @arguments variable value passed in by facetter
# @keyword hplot
#X p <- qplot(wt, mpg, data = mtcars)
#X p + facet_grid(~ cyl)
#X p + facet_grid(~ cyl, labeller = label_both)
label_both <- function(variable, value) paste(variable, value, sep = ": ")

# Label facets with parsed label.
# Parses the facet label, as if 
# 
# 
# @seealso \code{\link{plotmath}}
# @arguments variable name passed in by facetter
# @arguments variable value passed in by facetter
# @keyword hplot
#X mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#X qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
#X qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2, 
#X   labeller = label_parsed)
label_parsed <- function(variable, value) {
  plyr::llply(as.character(value), function(x) parse(text = x))
}

# Label facet with 'bquoted' expressions
# Create facet labels which contain the facet label in a larger expression
# 
# See \code{\link{bquote}} for details on the syntax of the argument.  The
# label value is x. 
# 
# @arguments expression to use
# @seealso \code{\link{plotmath}}
# @keyword hplot
#X p <- qplot(wt, mpg, data = mtcars)
#X p + facet_grid(~ vs, labeller = label_bquote(alpha ^ .(x)))
#X p + facet_grid(~ vs, labeller = label_bquote(.(x) ^ .(x)))
label_bquote <- function(expr = beta ^ .(x)) {
  quoted <- substitute(expr)
  
  function(variable, value) {
    value <- as.character(value)
    lapply(value, function(x)
      eval(substitute(bquote(expr, list(x = x)), list(expr = quoted))))
  }
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
