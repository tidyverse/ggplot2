\name{stat_qq}
\alias{stat_qq}
\title{Calculation for quantile-quantile plot.}
\usage{
  stat_qq(mapping = NULL, data = NULL, geom = "point",
    position = "identity", distribution = qnorm,
    dparams = list(), na.rm = FALSE, ...)
}
\arguments{
  \item{quantiles}{Quantiles to compute and display}

  \item{dist}{Distribution function to use, if x not
  specified}

  \item{dparams}{Parameters for distribution function}

  \item{...}{Other arguments passed to distribution
  function}
}
\value{
  a data.frame with additional columns:
  \item{sample}{sample quantiles}
  \item{theoretical}{theoretical quantiles}
}
\description{
  Calculation for quantile-quantile plot.
}
\examples{
# From ?qqplot
y <- rt(200, df = 5)
qplot(sample = y, stat="qq")

# qplot is smart enough to use stat_qq if you use sample
qplot(sample = y)
qplot(sample = precip)

qplot(sample = y, dist = qt, dparams = list(df = 5))

df <- data.frame(y)
ggplot(df, aes(sample = y)) + stat_qq()
ggplot(df, aes(sample = y)) + geom_point(stat = "qq")

# Use fitdistr from MASS to estimate distribution params
library(MASS)
params <- as.list(fitdistr(y, "t")$estimate)
ggplot(df, aes(sample = y)) + stat_qq(dist = qt, dparam = params)

# Using to explore the distribution of a variable
qplot(sample = mpg, data = mtcars)
qplot(sample = mpg, data = mtcars, colour = factor(cyl))
}

