#' Get, set and update themes.
#' 
#' Use \code{theme_update} to modify a small number of elements of the current
#' theme or use \code{theme_set} to completely override it.
#' 
#' @param ... named list of theme settings
#' @export theme_update theme_set theme_get
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
#' @param axis.line line along axis
#' @param axis.text.x x axis label
#' @param axis.text.y y axis label
#' @param axis.ticks axis tick marks
#' @param axis.ticks.length tick mark length
#' @param axis.ticks.margin tick mark  margin spacing
#' @param axis.title.x horizontal tick labels
#' @param axis.title.y vertical tick labels
#' @param legend.background background of legend
#' @param legend.margin extra space added around legend (both width or height
#'   depending on orientation of legend)
#' @param legend.key background underneath legend keys
#' @param legend.key.size key background size 
#' @param legend.key.height key background height
#' @param legend.key.width key background width
#' @param legend.text legend labels
#' @param legend.text.align alignment of legend labels
#' @param legend.title legend name
#' @param legend.title.align alignment of legend title
#' @param legend.position A string or numeric vector specifying the position of guides (legends).
#'    Possible values are: "left", "right", "bottom", "top", and two-element numeric vector.
#' @param legend.justification alignment of legend 
#' @param legend.direction horizontal or vertical
#' @param legend.box A string specifying the direction of multiple guides. Possible string values are: "horizontal" and "vertical". 
#' @param panel.background background of panel
#' @param panel.border border around panel
#' @param panel.margin margin around facet panels
#' @param panel.grid.major major grid lines
#' @param panel.grid.minor minor grid lines
#' @param plot.background background of the entire plot
#' @param plot.title plot title
#' @param plot.margin plot margins
#' @param strip.background background of facet labels
#' @param strip.text.x text for horizontal strips
#' @param strip.text.y text for vertical strips
#'
#' @export
#' @examples
#' p <- qplot(mpg, wt, data = mtcars)
#' p 
#' p + opts(panel_background = theme_rect(colour = "pink"))
#' p + theme_bw()
#'
#' # Scatter plot of gas milage by vehicle weight
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + geom_abline(intercept = 37, slope = -5) 
#' # Calculate correlation coefficient
#' with(mtcars, cor(wt, mpg, use = "everything", method = "pearson"))
#' #annotate the plot
#' p + geom_abline(intercept = 37, slope = -5) + 
#' geom_text(data = data.frame(), aes(4.5, 30, label = "Pearson-R = -.87"))
#'
#' # Change the axis labels
#' # Original plot
#' p
#' p + xlab("Vehicle Weight") + ylab("Miles per Gallon")
#' # Or
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#' 
#' # Add a title to the plot
#' p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
#' # Change title appearance
#' p <- p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
#' p + opts(plot.title = theme_text(size = 20))
#' p + opts(plot.title = theme_text(size = 20, colour = "Blue"))
#'
#' # Changing plot look with themes
#' m <- ggplot(movies, aes(x = rating)) + geom_histogram()
#' #default is theme_grey()
#' m 
#' # Compare with
#' m + theme_bw()
#' 
#' # Manipulate Axis Attributes
#' m + opts(axis.line = theme_segment())
#' m + opts(axis.line = theme_segment(colour = "red", linetype = "dotted"))
#' m + opts(axis.text.x = theme_text(colour = "blue"))
#' m + opts(axis.text.y = theme_blank())
#' m + opts(axis.ticks = theme_segment(size = 2))
#' m + opts(axis.title.y = theme_text(size = 20, angle = 90))
#' m + opts(axis.title.x = theme_blank())
#' m + opts(axis.ticks.length = unit(.85, "cm"))
#'
#' # Legend Attributes
#' z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
#' z
#' z + opts(legend.position = "none")
#' z + opts(legend.position = "bottom")
#' # Or use relative coordinates between 0 and 1
#' z + opts(legend.position = c(.5, .5))
#  # Add a border to the whole legend
#' z + opts(legend.background = theme_rect())
#' # Legend margin controls extra space around outside of legend:
#' z + opts(legend.background = theme_rect(), legend.margin = unit(1, "cm"))
#' z + opts(legend.background = theme_rect(), legend.margin = unit(0, "cm"))
#' # Or to just the keys
#' z + opts(legend.key = theme_rect())
#' z + opts(legend.key = theme_rect(fill = "yellow"))
#' z + opts(legend.key.size = unit(2.5, "cm"))
#' z + opts(legend.text = theme_text(size = 20, colour = "red", angle = 45))
#' z + opts(legend.title = theme_text(face = "italic"))
#'
#' # To change the title of the legend use the name argument
#' # in one of the scale options
#' z + scale_colour_brewer(name = "My Legend")
#' z + scale_colour_grey(name = "Number of \nCylinders")
#'
#' # Panel and Plot Attributes
#' z + opts(panel.background = theme_rect())
#' z + opts(panel.background = theme_rect(fill = "black"))
#' z + opts(panel.border = theme_rect(linetype = "dashed"))
#' z + opts(panel.grid.major = theme_line(colour = "blue"))
#' z + opts(panel.grid.minor = theme_line(colour = "red", linetype = "dotted"))
#' z + opts(panel.grid.major = theme_line(size = 2))
#' z + opts(plot.background = theme_rect())
#' z + opts(plot.background = theme_rect(fill = "grey"))
#'
#' # Faceting Attributes
#' k <- ggplot(diamonds, aes(carat, ..density..)) +  geom_histogram(binwidth = 0.2) +
#' facet_grid(. ~ cut)
#' k + opts(strip.background = theme_rect(colour = "purple", fill = "pink", size = 3, linetype = "dashed"))
#' k + opts(strip.text.x = theme_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5))
#' k + opts(panel.margin = unit(5, "lines"))
#' k + opts(panel.margin = unit(0, "lines"))
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

#' @S3method print theme
print.theme <- function(x, ...) {
  call <- attr(x, "call")
  print(call)
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x) {
  defaults(x$options, theme_get())
}

##' Update contents of a theme
##'
##' @title Update theme param
##' @param name name of a theme element
##' @param ... Pairs of name and value of theme parameters.
##' @return Updated theme element
##' @export
##' @examples
##' x <- theme_text(size = 15)
##' update_element(x, colour = "red")
##' # Partial matching works
##' update_element(x, col = "red")
##' # So does positional
##' update_element(x, "Times New Roman")
##' # And it throws an error if you use an argument that doesn't exist
##' update_element(x, noargument = 12)
##' # Or multiple arguments with the same name
##' update_element(x, size = 12, size = 15)
##' 
##' # Will look up element if given name
##' update_element("axis.text.x", colour = 20)
##' # Throws error if incorrectly named
##' update_element("axis.text", colour = 20)
update_element <- function(name, ...) {
 if (is.character(name)) {
   ele <- theme_get()[[name]]
   if (is.null(ele)) {
     stop("Could not find theme element ", name, call. = FALSE)
   }
 } else {
   ele <- name
 }

 call <- attr(ele, "call")
 stopifnot(!is.null(call))

 # Partial matching of named ... args with full names
 f <- eval(call[[1]])
 new_args <- match.call()
 new_args$name <- NULL
 new_args <- as.list(match.call(f, new_args)[-1])

 # Combine old call with new args
 old <- as.list(call)

 # evaluate old args in its env
 evaled_old_args <- llply(names(old[-1]), get, environment(ele))
 names(evaled_old_args) <- names(old[-1])
 # replace premise with evaluated vars
 old <- modifyList(old, evaled_old_args)

 eval(as.call(modifyList(old, new_args)))
}
