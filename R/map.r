# Convert map files into useful data.frames
#  * point level data
#  * area level data
# 
# Utilities to merge together, along with data

# try_require("maps","mapproj","maptools")
#
#
#geom_polylist <- function(coords, ...) {
#  def <- function(x, default=1) if (is.null(x)) default else x
#
#  order <- as.integer(def(attr(coords, "plotOrder"), 1:length(coords)))
#  grobs <- lapply(order, function(i) geom_mappoly(coords[[i]], ...))
#  
#  do.call(gList, grobs)
#}
#
#geom_mappoly <- function (coords, gp=gpar(fill="grey80", col="white"), gphollow=gpar(fill="blue", col=NA), delta=100, projection="", ...) {
#  def <- function(x, default=1) if (is.null(x)) default else x
#
#  nParts <- def(attr(coords, "nParts"))
#  from <-   def(attr(coords, "pstart")$from)
#  to <-     def(attr(coords, "pstart")$to, dim(coords)[1])
#  order <-  def(attr(coords, "plotOrder"), 1:nParts)
#  
#  grobPart <- function(i) {
#    gp <- if (attr(coords, "ringDir")[i] == 1) gp else gphollow
#    
#    coords <- coords[from[i]:to[i], ]
#    coords <- mapthin(list(x=coords[,1], y=coords[,2]), delta)
#    #coords <- mapproject(coords$x, coords$x, projection=projection, ...)
#    print(length(coords$x))
#
#    polygonGrob(coords$x, coords$y, gp=gp, default="native")
#  }
#  
#  gTree(children= do.call(gList, lapply(order, grobPart)))
#}
#
## Collapse polylist into a data frame that can be dealt with more easily.
#collapse_polylist <- function(pl) {
#  
#}
#
##library(maptools)
##pumap2 <- pumap
#thin <- function(x, p=0.05) {
#  n <- nrow(pumap[[x]])
#  b <- 1/p*(0:(round(n*p)-1))+1
#  
##  b <- sample(n, round(n*p), replace=F)
#  pumap2[[x]] <<- (pumap[[x]])[sort(b, decreasing=T),]
##  pumap2[[x]][,1] <<- pumap2[[x]][,1] *xscale + xoff
##  pumap2[[x]][,2] <<- pumap2[[x]][,2] *yscale + yoff
#  attr(pumap2[[1]],"pstart") <<- list(from=1, to=2)
#  attr(pumap2[[1]],"plotOrder") <<- attr(pumap[[1]],"plotOrder")
#  attr(pumap2[[1]],"bbox") <<- attr(pumap[[1]],"bbox")
#  attr(pumap2[[1]],"nParts") <<- attr(pumap[[1]],"nParts")
#  attr(pumap2[[1]],"ringDir") <<- attr(pumap[[1]],"ringDir")
#  attr(pumap2[[1]],"area") <<- attr(pumap[[1]],"area")
#  attr(pumap2[[1]],"centroid") <<- attr(pumap[[1]],"centroid")
#
#    pumap2[[x]] <<- (pumap[[x]])[sort(b, decreasing=T),]
#  #  pumap2[[x]][,1] <<- pumap2[[x]][,1] *xscale + xoff
#  #  pumap2[[x]][,2] <<- pumap2[[x]][,2] *yscale + yoff
#  attributes(new) <- attributes(x, c("bbox", "pstart", "ringDir", "area", "centroid", "plotOrder", "shp.type"))
#  attr(new, "nParts") <- length(new[[x]])
#
#}
#
##foo <- sapply(1:532, thin)
##if (!exists("x")) x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
#
##thin.one <- function(x, delta=1)  mapthin(data.frame(x=x[,1], x=x[,2]), delta=delta)
##thin.one(x$Shapes[[56]]$verts, delta=.01)
#
#