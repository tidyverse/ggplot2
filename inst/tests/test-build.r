# Test the complete path from plot specification to rendered data
context("Plot building")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

p <- ggplot(df, aes(x, y, colour = z)) + geom_point() + 
  scale_colour_manual(values = c("blue", "red", "yellow"))
ggplot_build(p)[[1]][[1]]

p <- ggplot(df, aes(z, x)) + geom_point()
ggplot_build(p)[[1]][[1]]

p <- ggplot(df, aes(z)) + geom_bar()
ggplot_build(p)[[1]][[1]]