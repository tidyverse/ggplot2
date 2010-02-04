# Fortify a map
# Fortify method for map objects
# 
# This function turns a map into a data frame than can more easily be
# plotted with ggplot2.
# 
# @arguments map object
# @arguments ignored
# @arguments ignored
# @keywords hplot
#X if (require(maps)) {
#X ca <- map_data("county", "ca")
#X qplot(long, lat, data = ca, geom="polygon", group = group)
#X tx <- map_data("county", "texas")
#X qplot(long, lat, data = tx, geom="polygon", group = group, 
#X  colour = I("white"))
#X }
fortify.map <- function(model, data, ...) {
  df <- as.data.frame(model[c("x", "y")])
  names(df) <- c("long", "lat")
  df$group <- cumsum(is.na(df$long) & is.na(df$lat)) + 1
  df$order <- 1:nrow(df)
  
  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df[complete.cases(df$lat, df$long), ]
}

# Map borders.
# Create a layer of map borders
# 
# @arguments map data, see \code{\link[maps]{map}} for details
# @arguments map region
# @arguments fill colour
# @arguments border colour
# @arguments other arguments passed on to \code{\link{geom_polygon}}
# @keywords hplot
#X if (require(maps)) {
#X ia <- map_data("county", "iowa")
#X mid_range <- function(x) mean(range(x))
#X seats <- ddply(ia, .(subregion), plyr::colwise(mid_range, .(lat, long)))
#X ggplot(seats, aes(long, lat)) + 
#X   geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
#X   geom_text(aes(label = subregion), data = seats, size = 2, angle = 45)
#X
#X data(us.cities)
#X capitals <- subset(us.cities, capital == 2)
#X ggplot(capitals, aes(long, lat)) +
#X   borders("state") + 
#X   geom_point(aes(size = pop)) + 
#X   scale_area()
#X }
borders <- function(database = "world", regions = ".", fill = NA, colour = "grey50", ...) {
  df <- map_data(database, regions)
  geom_polygon(aes(long, lat, group = group), data = df, 
    fill = fill, colour = colour, ...)
}

# Map data
# Convert map to data frame
# 
# @arguments map name
# @arguments region name
# @keywords hplot
#X if (require(maps)) {
#X states <- map_data("state")
#X arrests <- USArrests
#X names(arrests) <- tolower(names(arrests))
#X arrests$region <- tolower(rownames(USArrests))
#X 
#X choro <- merge(states, arrests, sort = FALSE, by = "region")
#X choro <- choro[order(choro$order), ]
#X qplot(long, lat, data = choro, group = group, fill = assault,
#X   geom="polygon")
#X qplot(long, lat, data = choro, group = group, fill = assault / murder,
#X   geom="polygon")
#X }
map_data <- function(map, region = ".") {
  fortify(map(map, region, plot = FALSE, fill = TRUE))
}
