\name{geom_boxplot}
\alias{geom_boxplot}
\title{Box and whiskers plot.}
\usage{geom_boxplot(mapping = NULL, data = NULL, stat = "boxplot",
    position = "dodge", outlier.colour = "black",
    outlier.shape = 16, outlier.size = 2, ...)}

\description{
  Box and whiskers plot.
}
\seealso{\code{\link{stat_quantile}} to view quantiles conditioned on a
continuous variable,  \code{\link{geom_jitter}} for another way to look 
at conditional distributions"}
\arguments{
  \item{outlier.colour}{colour for outlying points}
  \item{outlier.shape}{shape of outlying points}
  \item{outlier.size}{size of outlying points}
}
\examples{p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_boxplot()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")

p + geom_boxplot() + geom_jitter()
p + geom_boxplot() + coord_flip()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +
coord_flip()

p + geom_boxplot(outlier.colour = "green", outlier.size = 3)

# Add aesthetic mappings
# Note that boxplots are automatically dodged when any aesthetic is 
# a factor
p + geom_boxplot(aes(fill = cyl))
p + geom_boxplot(aes(fill = factor(cyl)))
p + geom_boxplot(aes(fill = factor(vs)))
p + geom_boxplot(aes(fill = factor(am)))

# Set aesthetics to fixed value
p + geom_boxplot(fill = "grey80", colour = "#3366FF")
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot", 
colour = I("#3366FF"))

# Scales vs. coordinate transforms -------
# Scale transformations occur before the boxplot statistics are computed.
# Coordinate transformations occur afterwards.  Observe the effect on the
# number of outliers.
m <- ggplot(movies, aes(y = votes, x = rating,
group = round_any(rating, 0.5)))
m + geom_boxplot()
m + geom_boxplot() + scale_y_log10()
m + geom_boxplot() + coord_trans(y = "log10")
m + geom_boxplot() + scale_y_log10() + coord_trans(y = "log10")

# Boxplots with continuous x:
# Use the group aesthetic to group observations in boxplots
qplot(year, budget, data = movies, geom = "boxplot")
qplot(year, budget, data = movies, geom = "boxplot", 
group = round_any(year, 10, floor))

# Using precomputed statistics
# generate sample data
abc <- adply(matrix(rnorm(100), ncol = 5), 2, quantile, c(0, .25, .5, .75, 1))
b <- ggplot(abc, aes(x = X1, ymin = `0\%`, lower = `25\%`, middle = `50\%`, upper = `75\%`, ymax = `100\%`)) 
b + geom_boxplot(stat = "identity")
b + geom_boxplot(stat = "identity") + coord_flip()
b + geom_boxplot(aes(fill = X1), stat = "identity")}

