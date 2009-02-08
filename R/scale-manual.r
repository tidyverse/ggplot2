ScaleManual <- proto(ScaleDiscrete, {  
  doc <- TRUE
  common <- c("colour","fill","size","shape","linetype")
  values <- c()
  
  new <- function(., name=NULL, values=NULL, variable="x", limits = NULL, breaks = NULL, labels = NULL, formatter = identity) {
    .$proto(name=name, values=values, .input=variable, .output=variable, limits = limits, breaks = breaks, .labels = labels, formatter = formatter)
  }

  map <- function(., values) {
    .$check_domain()

    values <- as.character(values)
    values[is.na(values)] <- "NA"

    if (.$has_names()) {
      .$output_breaks()[values]
    } else {
      .$output_breaks()[match(values, .$input_set())]
    }
  }

  has_names <- function(.) !is.null(names(.$output_breaks()))

  output_breaks <- function(.) .$values
  output_set <- function(.) .$values
  labels <- function(.) {
    if (!is.null(.$.labels)) return(.$.labels)
    if (.$has_names()) names(.$output_breaks()) else .$.domain
  }

  # Documentation -----------------------------------------------

  objname <- "manual"
  desc <- "Create your own discrete scale"
  icon <- function(.) textGrob("DIY", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl))

    p + scale_colour_manual(values = c("red","blue", "green"))
    p + scale_colour_manual(values = c("8" = "red","4" = "blue","6" = "green"))
  }
  
})
