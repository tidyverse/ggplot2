#' Translating between qplot and Graphics Production Library (GPL)
#' 
#' The Grammar of Graphics uses two specifications. A concise format is used to
#' caption figures, and a more detailed xml format stored on disk.
#'
#' @name translate_qplot_gpl
#' @examples
#'
#' # The following example of the concise format is adapted from Figure 1.5, 
#' # page 13, of Leland Wilkinson's "The Grammar of Graphics."
#' # Springer, 2nd edition, 2005.
#' 
#' DATA: source("demographics")
#' DATA: longitude, latitude = map(source("World"))
#' TRANS: bd = max(birth - death, 0)
#' COORD: project.mercator()
#' ELEMENT: point(position(lon * lat), size(bd), color(color.red))
#' ELEMENT: polygon(position(longitude * latitude))
#' 
#' # This is relatively simple to adapt to the syntax of ggplot2:
#'
#' # ggplot() is used to specify the default data and default aesthetic mappings.
#' # Data is provided as standard R data.frames existing in the global environment;
#' # it does not need to be explicitly loaded. We also use a slightly
#' # different world dataset, with columns lat and long. This lets us use the
#' # same aesthetic mappings for both datasets. Layers can override the default
#' # data and aesthetic mappings provided by the plot.
#' 
#' # We replace TRANS with an explicit transformation by R code.
#'
#' # ELEMENTs are replaced with layers, which explicitly specify the data
#' # source. Each geom has a default statistic which is used to transform the
#' # data prior to plotting. For the geoms in this example, the default statistic
#' # is the identity function. Fixed aesthetics (the colour red in this example)
#' # are supplied as additional arguments to the layer, rather than as special
#' # constants.
#'
#' # The SCALE component has been omitted from this example (so that the
#' # defaults are used). In both the ggplot2 and GoG examples, scales are
#' # defined by default. In ggplot you can override the defaults by adding a
#' # scale object, e.g., scale colour or scale size.
#'
#' # COORD uses a slightly different format. In general, most of the components
#' # specifications in ggplot are slightly different to those in GoG, in order to
#' # be more familiar to R users.
#'
#' # Each component is added together with + to create the final plot.
#'
#' # Resulting ggplot2 code:
#' demographics <- transform(demographics, bd = pmax(birth - death, 0)) 
#' p <- ggplot(demographic, aes(lon, lat))
#' p <- p + geom_polyogon(data = world)
#' p <- p + geom_point(aes(size = bd), colour = "red") 
#' p <- p + coord_map(projection = "mercator")
#' print(p)
NULL