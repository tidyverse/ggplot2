GeomRibbon <- proto(GeomInterval, {
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="grey60", fill="grey80", size=1, linetype=1)
  guide_geom <- function(.) "tile"

  adjust_scales_data <- function(., scales, data) {
    if (!"y" %in% scales$input()) {
      scales$add(ScaleContinuous$new(variable="y"))  
    }

    y <- scales$get_scales("y")
    y$train(data$min)
    y$train(data$y)
    y$train(data$max)
  }

  draw <- function(., data, scales, coordinates, ...) {
    data <- data[complete.cases(data[, c("x","min","max")]), ]
    tb <- with(data,
      coordinates$munch(data.frame(x=c(x, rev(x)), y=c(max, rev(min))))
    )
    
    with(data, ggname(.$my_name(), gTree(children=gList(
      ggname("fill", polygonGrob(
        tb$x, tb$y,
        default.units="native",
        gp=gpar(fill=fill, col=NA) 
      )),
      ggname("outline", polygonGrob(
        tb$x, tb$y,
        default.units="native",
        gp=gpar(fill=NA, col=colour, lwd=size * .pt, lty=linetype)
      ))
    ))))
  }

  # Documetation -----------------------------------------------
  objname <- "ribbon"
  desc <- "Ribbons, y range with continuous x values"
  
  icon <- function(.) {
    polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0), c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7), gp=gpar(fill="grey60", col=NA))
  }
  
  seealso <- list(
    geom_bar = "Discrete intervals (bars)",
    geom_linerange = "Discrete intervals (lines)",
    geom_polygon = "General polygons"
  )
  
  examples <- function(.) {
    # Generate data
    huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
    huron$decade <- round_any(huron$year, 10, floor)

    h <- ggplot(huron, aes(x=year))

    h + geom_ribbon(aes(min=0, max=level))
    h + geom_area(aes(y = level))

    # Add aesthetic mappings
    h + geom_ribbon(aes(min=level-1, max=level+1))
    h + geom_ribbon(aes(min=level-1, max=level+1)) + geom_line(aes(y=level))
    
    # Another data set, with multiple y's for each x
    m <- ggplot(movies, aes(y=votes, x=year)) 
    (m <- m + geom_point())
    
    # The default summary isn't that useful
    m + stat_summary(geom="ribbon")
    m + stat_summary(geom="ribbon", fun=stat_median_hilow)
    
    # Use qplot instead
    qplot(year, level, data=huron, geom=c("area", "line"))
  }  
})

GeomArea <- proto(GeomRibbon,{
  default_aes <- function(.) aes(colour="grey60", fill="grey80", min=0, max=y, size=0.5, linetype=1)
  default_pos <- function(.) PositionStack
  required_aes <- c("x", "y", "min", "max")

  # Documetation -----------------------------------------------
  objname <- "area"
  desc <- "Area plots"

  icon <- function(.) {
    polygonGrob(c(0, 0,0.3, 0.5, 0.8, 1, 1), c(0, 1,0.5, 0.6, 0.3, 0.8, 0), gp=gpar(fill="grey60", col=NA))
  }


  details <- "<p>An area plot is the continuous analog of a stacked bar chart (see geom_bar), and can be used to show how composition of the whole varies over the range of x.  Choosing the order in which different components is stacked is very important, as it becomes increasing hard to see the individual pattern as you move up the stack.</p>\n<p>An area plot is a special case of geom_ribbon, where the minimum of the range is fixed to 0, and the position adjustment defaults to position_stacked.</p>"


  examples <- function(.) {
    # Examples to come
  }
})
