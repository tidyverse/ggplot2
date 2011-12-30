#' Label facets with their value.
#' This is the default labelling scheme.
#' 
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(. ~ cyl, labeller = label_value)
label_value <- function(variable, value) as.character(value)

#' Label facets with value and variable.
#' 
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(. ~ cyl, labeller = label_both)
label_both <- function(variable, value) paste(variable, value, sep = ": ")

#' Label facets with parsed label.
#' 
#' @seealso \code{\link{plotmath}}
#' @param variable variable name passed in by facetter
#' @param value variable value passed in by facetter
#' @family facet labellers
#' @export
#' @examples
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
#' qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2, 
#'   labeller = label_parsed)
label_parsed <- function(variable, value) {
  llply(as.character(value), function(x) parse(text = x))
}

#' Label facet with 'bquoted' expressions
#' 
#' See \code{\link{bquote}} for details on the syntax of the argument.  The
#' label value is x. 
#' 
#' @param expr labelling expression to use
#' @family facet labellers
#' @seealso \code{\link{plotmath}}
#' @export
#' @examples
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(.(x) ^ .(x)))
label_bquote <- function(expr = beta ^ .(x)) {
  quoted <- substitute(expr)
  
  function(variable, value) {
    value <- as.character(value)
    lapply(value, function(x)
      eval(substitute(bquote(expr, list(x = x)), list(expr = quoted))))
  }
}

# Grob for strip labels
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
