#' Update axis/legend labels
#' 
#' @param p plot to modify
#' @param labels named list of new labels
#' @export
#' @examples
#' p <- qplot(mpg, wt, data = mtcars)
#' update_labels(p, list(x = "New x"))
#' update_labels(p, list(x = expression(x / y ^ 2)))
#' update_labels(p, list(x = "New x", y = "New Y"))
#' update_labels(p, list(colour = "Fail silently"))
update_labels <- function(p, labels) {
  p <- plot_clone(p)
  p + opts(labels = labels)
}

#' Change axis labels and legend titles
#' 
#' @param ... a list of new names in the form aesthetic = "new name"
#' @aliases labs xlab ylab
#' @export labs xlab ylab
#' @examples
#' p <- qplot(mpg, wt, data = mtcars)
#' p + labs(x = "New x label")
#' p + xlab("New x label")
#' p + ylab("New y label")
#'
#' # This should work indepdendently of other functions that modify the 
#' # the scale names
#' p + ylab("New y label") + ylim(2, 4)
#' p + ylim(2, 4) + ylab("New y label")
#'
#' # The labs function also modifies legend labels
#' p <- qplot(mpg, wt, data = mtcars, colour = cyl)
#' p + labs(colour = "Cylinders")
#'
#' # Can also pass in a list, if that is more convenient
#' p + labs(list(x = "X", y = "Y")) 
labs <- function(...) {
  args <- list(...)
  if (is.list(args[[1]])) args <- args[[1]]
  structure(args, class = "labels")
}

xlab <- function(label) {
  labs(x = label)
}
ylab <- function(label) {
  labs(y = label)
}

# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  remove_dots <- function(x) {
    gsub("\\.\\.([a-zA-z._]+)\\.\\.", "\\1", x)
  }
  
  lapply(mapping, function(x) remove_dots(deparse(x)))
}