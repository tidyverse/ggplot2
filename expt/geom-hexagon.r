# GeomHexagon <- proto(Geom, {
#   objname <- "hexagon"
#   desc <- "Create hexagon binning of data points as created by Dan Carr. This geom is useful for scatterplots with a lot of overplotting.  It bins the region into hexagons, counts the number of points in each hexagonal bin and then plots them."
# 
#   default_stat <- function(.) StatIdentity
#   default_aes <- function(.) aes(weight=1)
#   seealso <- "\\code{\\link[hexbin]{grid.hexagon}}, \\code{\\link[hexbin]{geom_2density}} for another way of dealing with overplotting"
#   desc_params <- list(
#     xbins = "number of bins to use",
#     "..." =  "other arguments passed to \\code{\\link[hexbin]{grid.hexagons}}"
#   )
# 
#   draw <- function(., data, scales, coordinates, xbins=30, ...) {
#     try_require("hexbin")
#     
#     if (!length(unique(data$weight) == 1)) {
#       hexes <- hexbin(data$x, data$y, xbins=xbins, ID=TRUE)
#       cell <- hexes@cID
#       hexes@count <- as.vector(tapply(weight, cell, sum))
#     } else {
#       hexes <- hexbin(data$x, data$y, xbins=xbins)  
#     }
# 
#     grid.grabExpr(grid.hexagons(hexes, ...))
#   }
# 
#   examples <- function(.) {
#     # Generate data
#     # Add aesthetic mappings
#     # Change scale
#     # Set aesthetics to fixed value
#     # Use qplot instead
#   
#     m <- ggplot(movies, aes(y=length, x=rating))
#     m + geom_hexagon()
#     m + geom_hexagon(xbins=50)
#     m + geom_hexagon(style="lattice")
#     m + geom_hexagon(aes(weight=votes))
#   }  
# })
