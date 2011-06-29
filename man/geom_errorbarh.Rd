\name{geom_errorbarh}
\alias{geom_errorbarh}
\title{Horizontal error bars...}
\usage{geom_errorbarh(mapping, data, stat="identity",
    position="identity", ...)}

\description{
  Horizontal error bars
}
\seealso{\code{\link{geom_errorbar}}: vertical error bars}
\arguments{
  \item{height}{height of errorbars}
}
\examples{df <- data.frame(
trt = factor(c(1, 1, 2, 2)), 
resp = c(1, 5, 3, 4), 
group = factor(c(1, 2, 1, 2)), 
se = c(0.1, 0.3, 0.3, 0.2)
)

# Define the top and bottom of the errorbars

p <- ggplot(df, aes(resp, trt, colour = group))
p + geom_point() +
geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
p + geom_point() +
geom_errorbarh(aes(xmax = resp + se, xmin = resp - se, height = .2))}
