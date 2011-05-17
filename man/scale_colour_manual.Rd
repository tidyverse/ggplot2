\name{scale_colour_manual}
\alias{scale_colour_manual}
\title{Create your own discrete scale.}
\usage{scale_colour_manual(..., values)}

\description{
  Create your own discrete scale.
}
\examples{p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))

p + scale_colour_manual(values = c("red","blue", "green"))
p + scale_colour_manual(
values = c("8" = "red","4" = "blue","6" = "green"))

# As with other scales you can use breaks to control the appearance
# of the legend
cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols)
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
labels = c("four", "six", "eight"))

# And limits to control the possible values of the scale
p + scale_colour_manual(values = cols, limits = c("4", "8"))
p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))}
