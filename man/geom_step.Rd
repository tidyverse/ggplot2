\name{geom_step}
\alias{geom_step}
\title{Connect observations by stairs.}
\usage{
  geom_step(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", direction = "hv", ...)
}
\arguments{
  \item{direction}{direction of stairs: 'vh' for vertical
  then horizontal, or 'hv' for horizontal then vertical}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Connect observations by stairs.
}
\examples{
# Simple quantiles/ECDF from examples(plot)
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
qplot(seq_along(x), x, data = df, geom="step", colour = trt)
}

