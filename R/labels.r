# Update axis/legend labels
# Change the scale names of an existing plot
# 
# @arguments plot
# @arguments named list of new labels
# @keyword internal
#X p <- qplot(mpg, wt, data = mtcars)
#X update_labels(p, list(x = "New x"))
#X update_labels(p, list(x = expression(x / y ^ 2)))
#X update_labels(p, list(x = "New x", y = "New Y"))
#X update_labels(p, list(colour = "Fail silently"))
update_labels <- function(p, labels) {
  p <- plot_clone(p)
  p + opts(labels = labels)
}

# Change axis labels and legend titles
# This is a convenience function that saves some typing when modifying the axis labels or legend titles
# 
# @arguments a list of new names in the form aesthetic = "new name"
# @alias xlab
# @alias ylab
#X p <- qplot(mpg, wt, data = mtcars)
#X p + labs(x = "New x label")
#X p + xlab("New x label")
#X p + ylab("New y label")
#X
#X # This should work indepdendently of other functions that modify the 
#X # the scale names
#X p + ylab("New y label") + ylim(2, 4)
#X p + ylim(2, 4) + ylab("New y label")
#X
#X # The labs function also modifies legend labels
#X p <- qplot(mpg, wt, data = mtcars, colour = cyl)
#X p + labs(colour = "Cylinders")
#X
#X # Can also pass in a list, if that is more convenient
#X p + labs(list(x = "X", y = "Y")) 
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
# This is used by ggplot and + to ensure consistent label formatting
# 
# @keyword internal
make_labels <- function(mapping) {
  remove_dots <- function(x) {
    gsub("\\.\\.([a-zA-z._]+)\\.\\.", "\\1", x)
  }
  
  lapply(mapping, function(x) remove_dots(deparse(x)))
}