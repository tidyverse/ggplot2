
# m x n
df <- data.frame(x = rep(c(-1, 1), each = 3), y = rep(-1:1, 2), z = 1:6)

ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)
ggplot(df, aes(x, y, fill = z)) + geom_raster(hpad = 0, vpad = 0) + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)

# m x 1, 1 x n
df <- data.frame(x = -1:1, y = 0, z = 1:3)
ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)
ggplot(df, aes(x, y, fill = z)) + geom_raster(hpad = 0, vpad = 0.25) + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)

df <- data.frame(x = 0, y = -1:1, z = 1:3)
ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)
ggplot(df, aes(x, y, fill = z)) + geom_raster(hpad = 0.25, vpad = 0) + geom_point(colour = "red") + xlim(-2, 2) + ylim(-2, 2)

