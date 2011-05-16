\name{geom_path}
\alias{geom_path}
\title{Connect observations in original order...}

\description{
  Connect observations in original order
}
\seealso{\code{\link{geom_line}}: Functional (ordered) lines;  
\code{\link{geom_polygon}}: Filled paths (polygons); 
\code{\link{geom_segment}}: Line segments
# Generate data
myear <- ddply(movies, .(year), colwise(mean, .(length, rating)))
p <- ggplot(myear, aes(length, rating))
p + geom_path()

# Add aesthetic mappings
p + geom_path(aes(size = year))
p + geom_path(aes(colour = year))

# Change scale
p + geom_path(aes(size = year)) + scale_size(range = c(1, 3))

# Set aesthetics to fixed value
p + geom_path(colour = "green")

# Control line join parameters
df <- data.frame(x = 1:3, y = c(4, 1, 9))
base <- ggplot(df, aes(x, y))
base + geom_path(size = 10)
base + geom_path(size = 10, lineend = "round")
base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")

# Use qplot instead
qplot(length, rating, data=myear, geom="path")

# Using economic data:
# How is unemployment and personal savings rate related?
qplot(unemploy/pop, psavert, data=economics)
qplot(unemploy/pop, psavert, data=economics, geom="path")
qplot(unemploy/pop, psavert, data=economics, geom="path", size=as.numeric(date))

# How is rate of unemployment and length of unemployment?
qplot(unemploy/pop, uempmed, data=economics)
qplot(unemploy/pop, uempmed, data=economics, geom="path")
qplot(unemploy/pop, uempmed, data=economics, geom="path") +
geom_point(data=head(economics, 1), colour="red") + 
geom_point(data=tail(economics, 1), colour="blue")
qplot(unemploy/pop, uempmed, data=economics, geom="path") +
geom_text(data=head(economics, 1), label="1967", colour="blue") + 
geom_text(data=tail(economics, 1), label="2007", colour="blue")

# geom_path removes missing values on the ends of a line.
# use na.rm = T to suppress the warning message
df <- data.frame(
x = 1:5,
y1 = c(1, 2, 3, 4, NA),
y2 = c(NA, 2, 3, 4, 5),
y3 = c(1, 2, NA, 4, 5),
y4 = c(1, 2, 3, 4, 5))
qplot(x, y1, data = df, geom = c("point","line"))
qplot(x, y2, data = df, geom = c("point","line"))
qplot(x, y3, data = df, geom = c("point","line"))
qplot(x, y4, data = df, geom = c("point","line"))

# Setting line type vs colour/size
# Line type needs to be applied to a line as a whole, so it can
# not be used with colour or size that vary across a line

x <- seq(0.01, .99, length=100)
df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
p <- ggplot(df, aes(x=x, y=y, group=group))

# Should work
p + geom_line(linetype = 2)
p + geom_line(aes(colour = group), linetype = 2)
p + geom_line(aes(colour = x))

# Should fail
should_stop(p + geom_line(aes(colour = x), linetype=2))}
\arguments{
  \item{lineend}{Line end style (round, butt, square)}
  \item{linejoin}{Line join style (round, mitre, bevel)}
  \item{linemitre}{Line mitre limit (number greater than 1)}
  \item{arrow}{Arrow specification, as created by ?arrow}
}
