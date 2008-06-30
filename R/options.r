# Set ggplot options
# Set global options for ggplot.
# 
# These are aliased into every plot object, so that \\code{p$grid.col} will
# return the default grid colour, unless it has been overriden for a particular
# plot object.  You can change the global options using the function, or the
# options for a specific plot by setting the values directly on the object.  See
# the examples for more details.
# 
# Colour settings:
# 
# \itemize{
#   \item axis.colour: axis text and line colour ("black")
#   \item background.colour: background text colour ("black"), used for title
#   \item background.fill:   background fill ("white")
#   \item grid.colour: plot grid colour ("white")
#   \item grid.fill:   plot grid background fill ("grey90")
# }
# 
# Strip settings
# 
# \itemize{
#   \item strip.text:   function with two arguments (variable, and value) used for
#     generating strip labels
#   \item strip.gp: graphic parameter settings for the strip
#   \item strip.text.gp:  graphic parameter settings for the strip text
# }
# 
# Legend settings
# 
# \itemize{
#   \item legend.position:   position of legend: "none" to hide legend;
#     "left", "right", "top", "bottom", for positioning outside of plot;
#     c(x, y) for positioning on top of plot
#   \item legend.justifcation: part of legend that position refers to
# }
# 
# Other settings:
# 
# \itemize{
#   \item aspect.ratio: aspect ratio of facets.  Set to \\code{NULL} to allow
#         to vary with device size
# }
# 
# @arguments list of options to get/set
# @returns previous settings
# @keyword manip 
# @alias ggopt
# @alias opts
# @alias update.ggplot
# @alias theme_default
# @alias theme_bw
#X ggopt(background.fill = "black", background.color ="white") # all new plots will use this
#X p <- qplot(total_bill, tip, facets = smoker ~ sex, data=tips)
#X p
#X p$background.fill = "white"
#X p
#X p$strip.text.gp <- gpar(col="red", fontsize=15)
#X p$strip.gp <- gpar(fill="black")
#X p$background.fill <- "black"
#X p$background.colour <- "white"
#X p$grid.colour <- "white"
#X p$grid.fill <- "grey50"
#X p # a very ugly plot!
#X ggopt(background.fill = "white", background.color ="black")
#X 
#X p <- qplot(wt, mpg, data=mtcars, colour=factor(cyl))
#X p + opts(legend.position = c(0.9,0.9))
#X (p <- p + opts(legend.position = c(0.5,0.5)))
#X p + opts(legend.justification = "centre")
#X 
#X DF <- data.frame(
#X   x=rnorm(20), 
#X   y=rnorm(20), 
#X   g1=rep(letters[1:2], 10),
#X   g2=rep(LETTERS[1:2], each=10)
#X )
#X 
#X (p <- qplot(x, y, data=DF, facets = g1 ~ g2))
#X 
#X p$strip.text <- function (variable, value) {
#X   greek <- c("A" = "alpha", "B" = "beta")[value]
#X   makelabel <- function (g) substitute(variable == greek, list(variable=as.name(variable), greek=as.name(g)))
#X 
#X   lapply(greek, makelabel)
#X }
#X 
#X p
.build_options <- function(opt) {
  class(opt) <- "options"
  
  function(...) {
    if (length(list(...)) == 0) return(opt)
    old <- opt
    opt <<- updatelist(opt, list(...))
    invisible(old)
  }
}

# Print options
# Uses str
# 
# @keyword internal 
print.options <- function(x, ...) str(x)

# Set ggplot theme.
# A theme is a list of options for \code{\link{ggopt}}. 
# 
# Use \code{ggtheme(defaulttheme)} to reset back to the
# default theme.
# 
# @arguments theme, a list of options for \code{\link{ggopt}}
# @keyword manip 
ggtheme <- function(theme) {
  do.call("ggopt", theme)
}

theme_default <- structure(list(
  aspect.ratio = NULL,
  auto.print = FALSE,
  axis.colour = "grey50",
  background.colour = "black",
  background.fill = "white",
  border.colour = "white",
  grid.colour = "white",
  grid.minor.colour = "grey95",
  grid.fill = "grey90",
  legend.position = "right",
  legend.justification = c("right", "top"),
  save = FALSE,
  strip.gp = gpar(col = "white", fill = "grey80", lwd=2),
  strip.text = function(variable, value) value, #paste(variable, value, sep=": "),
  strip.text.gp = gpar()
), class="options")
ggopt <- .build_options(theme_default)

opts <- function(...) {structure(list(...), class="options")}

theme_bw <- structure(list(
  grid.colour = "grey80",
  grid.minor.colour = "NA",
  grid.fill = "white",
  axis.colour = "black",
  border.colour = "black"
), class="options")

update.ggplot <- function(object, ...) {
  dots <- list(...)
  if (length(dots) == 1 && is.list(dots[[1]])) dots <- dots[[1]]
  
  structure(defaults(dots, object), class="ggplot")
}

.plot_store <- function() {
  .last_plot <- NULL
  
  list(
    get = function() .last_plot, 
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

# Set last plot
# Set last plot created or modified
# 
# @arguments plot to store
# @keyword internal
set_last_plot <- function(value) .store$set(value)


# Retrieve last plot modified/created.
# Whenever a plot is created or modified, it is recorded.
# 
# @seealso \code{\link{ggsave}}
# @keyword hplot
last_plot <- function() .store$get()