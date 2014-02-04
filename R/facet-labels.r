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

#' Label facets with a word wrapped label.
#' 
#' Uses \code{\link[base]{strwrap}} for line wrapping.
#' @param width integer, target column width for output.
#' @export
#' @seealso , \code{\link{labeller}}
#' @examples
#' set.seed(331)
#' x=runif(60)
#' y=rnorm(60)
#' speed=sample(c('Prime group', 'Rib group', 'No group'), 60, replace=TRUE)
#' group=sample(letters[1:3], 60, replace=TRUE)
#' 
#' df = data.frame(x=x, y=y, speed=as.factor(speed), group=as.factor(group))
#' group.names <- c('a'='First','b'='Second','c'="Don\'t")
#' 
#' ggplot(df, aes(x, y)) + geom_point() + facet_grid(speed ~ group, labeller=label_wrap_gen(3))
#' ggplot(df, aes(x, y)) + geom_point() + facet_grid(speed ~ group, labeller=labeller(speed=label_wrap_gen(3), group=group.names))
label_wrap_gen <- function(width = 25) {
  function(variable, value) {
    lapply(strwrap(as.character(value), width=width, simplify=FALSE), 
           paste, collapse="\n")
  }
}

#' Generic labeller function for facets
#' 
#' One-step function for providing methods or named character vectors
#' as labels in facets.
#'
#' @param keep.as.numbers logical, default TRUE. When FALSE, converts numeric values supplied as margins to the facet to characters.
#' @family facet labeller
#' @return Function to supply to \code{\link{facet_grid}} for the argument \code{labeller}.
#' @export 
#' @examples
#' numbers <- c(`4`='four', `6`='six', `8`='eight')
#' vs <- c(`0`='No vs', `1`='vs')
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + facet_grid(vs~cyl, labeller=labeller(cyl=numbers, vs=vs))
labeller <- function(keep.as.numeric=FALSE, ...) {
  args <- list(...)
  lbl <- function(variable, values) {
    res <- args[[variable]]
    if (is.numeric(values) & !keep.as.numeric) values <- as.character(values)
    #print(str(variable))
    #print(str(values))
    
    if (is.null(res)) {
      if (is.factor(values)) return(levels(values[drop=TRUE]))
      return(values)
    }
    if (is.function(res)) return(res(variable, values))
    if (is.logical(values)) values <- as.integer(values)+1
    if (is.factor(values)) values <- levels(values)[values]
    return(res[values])
  }
  return(lbl)
}



# Grob for strip labels
ggstrip <- function(text, horizontal=TRUE, theme) {
  text_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  label <- element_render(theme, text_theme, text)

  ggname("strip", absoluteGrob(
    gList(
      element_render(theme, "strip.background"),
      label
    ),
    width = grobWidth(label) + unit(0.5, "lines"),
    height = grobHeight(label) + unit(0.5, "lines")
  ))
}
