\name{labs}
\alias{labs}
\alias{xlab}
\alias{ylab}
\title{Change axis labels and legend titles...}
\usage{
  labs(...)
}
\arguments{
  \item{...}{a list of new names in the form aesthetic =
  "new name"}
}
\description{
  Change axis labels and legend titles
}
\examples{
p <- qplot(mpg, wt, data = mtcars)
p + labs(x = "New x label")
p + xlab("New x label")
p + ylab("New y label")

# This should work indepdendently of other functions that modify the
# the scale names
p + ylab("New y label") + ylim(2, 4)
p + ylim(2, 4) + ylab("New y label")

# The labs function also modifies legend labels
p <- qplot(mpg, wt, data = mtcars, colour = cyl)
p + labs(colour = "Cylinders")

# Can also pass in a list, if that is more convenient
p + labs(list(x = "X", y = "Y"))
}

