\name{geom_step}
\alias{geom_step}
\title{Connect observations by stairs.}
\usage{geom_step(mapping, data, stat="identity",
    position="identity", direction="hv", ...)}

\description{
  Connect observations by stairs.
}
\arguments{
  \item{direction}{direction of stairs: 'vh' for vertical then horizontal, or
'hv' for horizontal then vertical}
}
\examples{# Simple quantiles/ECDF from examples(plot)
x <- sort(rnorm(47))
qplot(seq_along(x), x, geom="step")

# Steps go horizontally, then vertically (default)
qplot(seq_along(x), x, geom="step", direction = "hv")
plot(x, type = "s")
# Steps go vertically, then horizontally
qplot(seq_along(x), x, geom="step", direction = "vh")
plot(x, type = "S")

# Also works with other aesthetics
df <- data.frame(
x = sort(rnorm(50)),
trt = sample(c("a", "b"), 50, rep = T)
)
qplot(seq_along(x), x, data = df, geom="step", colour = trt)}
