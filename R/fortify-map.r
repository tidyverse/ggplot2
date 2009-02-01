fortify.map <- function(model, data, ...) {
  df <- as.data.frame(model[c("x", "y")])
  df$group <- cumsum(is.na(df$x) & is.na(df$x)) + 1
  df$name <- model$names[df$group]
  df$region <- sapply(strsplit(model$names, ":"), "[", 1)[df$group]
  df[complete.cases(df), ]
}

borders <- function(database = "world", regions = ".", fill = NA, colour = "grey50", ...) {
  df <- fortify(map(database, regions, plot = FALSE, fill = TRUE))
  geom_polygon(aes(x = x, y = y, group = group), data = df, fill = fill, colour = colour, ...)
}