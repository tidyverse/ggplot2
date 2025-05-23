% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify-models.R
\name{fortify.lm}
\alias{fortify.lm}
\title{Supplement the data fitted to a linear model with model fit statistics.}
\usage{
\method{fortify}{lm}(model, data = model$model, ...)
}
\arguments{
\item{model}{linear model}

\item{data}{data set, defaults to data used to fit model}

\item{...}{not used by this method}
}
\value{
The original data with extra columns:
\item{.hat}{Diagonal of the hat matrix}
\item{.sigma}{Estimate of residual standard deviation when
corresponding observation is dropped from model}
\item{.cooksd}{Cooks distance, \code{\link[=cooks.distance]{cooks.distance()}}}
\item{.fitted}{Fitted values of model}
\item{.resid}{Residuals}
\item{.stdresid}{Standardised residuals}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This method is deprecated because using \code{broom::augment()} is a better
solution to supplement data from a linear model.
If you have missing values in your model data, you may need to refit
the model with \code{na.action = na.exclude}.
}
\examples{
\dontshow{if (require("broom")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mod <- lm(mpg ~ wt, data = mtcars)

# Show augmented model
head(augment(mod))
head(fortify(mod))

# Using augment to convert model to ready-to-plot data
ggplot(augment(mod), aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

# Colouring by original data not included in the model
ggplot(augment(mod, mtcars), aes(.fitted, .std.resid, colour = factor(cyl))) +
  geom_point()
\dontshow{\}) # examplesIf}
}
\keyword{internal}
