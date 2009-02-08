# library(maps)
# ca <- map_data("county", "ca")
# qplot(long, lat, data = ca, geom="polygon", group = group)
# tx <- map_data("county", "texas")
# qplot(long, lat, data = tx, geom="polygon", group = group, colour = I("white"))

fortify.map <- function(model, data, ...) {
  df <- as.data.frame(model[c("x", "y")])
  names(df) <- c("long", "lat")
  df$group <- cumsum(is.na(df$long) & is.na(df$long)) + 1
  df$order <- 1:nrow(df)
  
  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df
}

# ia <- map_data("county", "iowa")
# mid_range <- function(x) mean(range(x))
# seats <- ddply(ia, .(subregion), colwise(mid_range, .(lat, long)))
# ggplot(seats, aes(long, lat)) + 
#   geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
#   geom_text(aes(label = subregion), data = seats, size = 2, angle = 45)
#
# data(us.cities)
# capitals <- subset(us.cities, capital == 2)
# ggplot(capitals, aes(long, lat)) +
#   borders("state") + 
#   geom_point(aes(size = pop)) + 
#   scale_area()
borders <- function(database = "world", regions = ".", fill = NA, colour = "grey50", ...) {
  df <- map_data(database, regions)
  geom_polygon(aes(long, lat, group = group), data = df, 
    fill = fill, colour = colour, ...)
}

map_data <- function(map, region = ".") {
  fortify(map(map, region, plot = F, fill = T))
}

# states <- map_data("state")
# arrests <- USArrests
# names(arrests) <- tolower(names(arrests))
# arrests$region <- tolower(rownames(USArrests))
# 
# choro <- merge(states, arrests, sort = F, by = "region")
# choro <- choro[order(choro$order), ]
# qplot(long, lat, data = choro, group = group, fill = assault, geom="polygon")
# qplot(long, lat, data = choro, group = group, fill = assault / murder, geom="polygon")


# What do we need: 
#   * map source: database + region
#   * column to match on: 