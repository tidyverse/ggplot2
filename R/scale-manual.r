ScaleManual <- proto(ScaleDiscrete, {  
  common <- c("colour","fill","size","shape","linetype")
  .values <- c()
  
  new <- function(., name=NULL, values=NULL, variable="x", limits = NULL, labels = NULL) {
    .$proto(name=name, .values=values, .input=variable, .output=variable, limits = limits, .labels = labels)
  }

  map <- function(., values) {
    .$check_domain()
    if (.$has_names()) {
      .$domain_breaks()[as.character(values)]
    } else {
      .$domain_breaks()[match(as.character(values), .$domain())]
    }
  }

  has_names <- function(.) !is.null(names(.$domain_breaks()))

  breaks <- function(.) .$.values
  labels <- function(.) if (.$has_names()) names(.$domain_breaks()) else .$.domain

  # Documetation -----------------------------------------------

  objname <- "manual"
  desc <- "Simple way of manually controlling scale"
  icon <- function(.) textGrob("man", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl))

    p + scale_colour_manual(values = c("red","blue", "green"))
    p + scale_colour_manual(values = c("8" = "red","4" = "blue","6" = "green"))
  }
  
})
