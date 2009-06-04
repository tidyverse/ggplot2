GeomRibbon <- proto(Geom, {
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, alpha = 1)
  required_aes <- c("x", "ymin", "ymax")
  guide_geom <- function(.) "polygon"


  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, 
      c("x","ymin","ymax"), name = "geom_ribbon")
    data <- data[order(data$group, data$x), ]
    
    tb <- with(data,
      coordinates$munch(data.frame(x=c(x, rev(x)), y=c(ymax, rev(ymin))), scales)
    )
    
    with(data, ggname(.$my_name(), gTree(children=gList(
      ggname("fill", polygonGrob(
        tb$x, tb$y,
        default.units="native",
        gp=gpar(fill=alpha(fill, alpha), col=NA) 
      )),
      ggname("outline", polygonGrob(
        tb$x, tb$y,
        default.units="native",
        gp=gpar(fill=NA, col=colour, lwd=size * .pt, lty=linetype)
      ))
    ))))
  }

  # Documentation -----------------------------------------------
  objname <- "ribbon"
  desc <- "Ribbons, y range with continuous x values"
  
  icon <- function(.) {
    polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0), c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7), gp=gpar(fill="grey20", col=NA))
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

    h + geom_ribbon(aes(ymin=0, ymax=level))
    h + geom_area(aes(y = level))

    # Add aesthetic mappings
    h + geom_ribbon(aes(ymin=level-1, ymax=level+1))
    h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))
    
    # Another data set, with multiple y's for each x
    m <- ggplot(movies, aes(y=votes, x=year)) 
    (m <- m + geom_point())
    
    # The default summary isn't that useful
    m + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
    m + stat_summary(geom="ribbon", fun.data="median_hilow")
    
    # Use qplot instead
    qplot(year, level, data=huron, geom=c("area", "line"))
  }  
})

GeomArea <- proto(GeomRibbon,{
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, alpha = 1)
  default_pos <- function(.) PositionStack
  required_aes <- c("x", "y")

  reparameterise <- function(., df, params) {
    transform(df, ymin = 0, ymax = y)
  }

  # Documentation -----------------------------------------------
  objname <- "area"
  desc <- "Area plots"

  icon <- function(.) {
    polygonGrob(c(0, 0,0.3, 0.5, 0.8, 1, 1), c(0, 1,0.5, 0.6, 0.3, 0.8, 0), gp=gpar(fill="grey20", col=NA))
  }

  details <- "<p>An area plot is the continuous analog of a stacked bar chart (see geom_bar), and can be used to show how composition of the whole varies over the range of x.  Choosing the order in which different components is stacked is very important, as it becomes increasing hard to see the individual pattern as you move up the stack.</p>\n<p>An area plot is a special case of geom_ribbon, where the minimum of the range is fixed to 0, and the position adjustment defaults to position_stacked.</p>"


  examples <- function(.) {
    # Examples to come
  }
})
