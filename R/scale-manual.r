ScaleManual <- proto(ScaleDiscrete, {  
  doc <- TRUE
  common <- c("colour","fill","size","shape","linetype")
  values <- c()
  
  new <- function(., name=NULL, values=NULL, variable="x", limits = NULL, breaks = NULL, labels = NULL, formatter = identity) {
    b_and_l <- check_breaks_and_labels(breaks, labels)
    
    .$proto(name=name, values=values, .input=variable, .output=variable, limits = limits, breaks = b_and_l$breaks, .labels = b_and_l$labels, formatter = formatter)
  }

  map <- function(., values) {
    .$check_domain()

    values <- as.character(values)
    values[is.na(values)] <- "NA"
    input <- .$input_set()
    input[is.na(input)] <- "NA"
    
    if (.$has_names()) {
      values[!values %in% input] <- NA
      .$output_set()[values]
    } else {
      
      .$output_set()[match(values, input)]
    }
  }

  has_names <- function(.) !is.null(names(.$output_set()))

  input_breaks <- function(.) nulldefault(.$breaks, .$input_set())
  output_breaks <- function(.) .$map(.$input_breaks())

  output_set <- function(.) .$values
  labels <- function(.) {
    .$.labels %||% .$input_breaks()
  }

  # Documentation -----------------------------------------------

  objname <- "manual"
  desc <- "Create your own discrete scale"
  icon <- function(.) textGrob("DIY", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))

    p + scale_colour_manual(values = c("red","blue", "green"))
    p + scale_colour_manual(
      values = c("8" = "red","4" = "blue","6" = "green"))
    
    # As with other scales you can use breaks to control the appearance
    # of the legend
    cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
    p + scale_colour_manual(values = cols)
    p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
    p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
    p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
      labels = c("four", "six", "eight"))
    
    # And limits to control the possible values of the scale
    p + scale_colour_manual(values = cols, limits = c("4", "8"))
    p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
    
  }
  
})
