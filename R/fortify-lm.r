# Fortify a linear model with its data
# Supplement the data fitted to a linear model with model fit statistics.
# 
# The following statistics will be added to the data frame:
# 
# \itemize{
#   \item{.hat}{Diagonal of the hat matrix}
#   \item{.sigma}{Estimate of residual standard deviation when corresponding
#      observation is dropped from model}
#   \item{.cooksd}{Cooks distance, \code{\link{cooks.distance}}}
#   \item{.fitted}{Fitted values of model}
#   \item{.resid}{Residuals}
#  \item{.stdresid}{Standardised residuals}
# }
# 
# If you have missing values in your model data, you may need to refit 
# the model with \code{na.action = na.preserve}.
#
# @arguments linear model
# @arguments data set, defaults to data used to fit model
# @arguments not used
#X mod <- lm(mpg ~ wt, data = mtcars)
#X fortify(mod)
#X fortify(mod, mtcars)
#X 
#X plot(mod, which = 1)
#X qplot(.fitted, .resid, data = mod) + geom_hline() + geom_smooth(se = FALSE)
#X qplot(.fitted, .stdresid, data = mod) + geom_hline() + 
#X   geom_smooth(se = FALSE)
#X qplot(.fitted, .stdresid, data = fortify(mod, mtcars), 
#X   colour = factor(cyl))
#X qplot(mpg, .stdresid, data = fortify(mod, mtcars), colour = factor(cyl))
#X
#X plot(mod, which = 2)
#X # qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline()
#X
#X plot(mod, which = 3)
#X qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE)
#X
#X plot(mod, which = 4)
#X qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar",
#X  stat="identity")
#X
#X plot(mod, which = 5)
#X qplot(.hat, .stdresid, data = mod) + geom_smooth(se = FALSE)
#X ggplot(mod, aes(.hat, .stdresid)) + 
#X   geom_vline(size = 2, colour = "white", xintercept = 0) +
#X   geom_hline(size = 2, colour = "white", yintercept = 0) +
#X   geom_point() + geom_smooth(se = FALSE)
#X 
#X qplot(.hat, .stdresid, data = mod, size = .cooksd) + 
#X   geom_smooth(se = FALSE, size = 0.5)
#X
#X plot(mod, which = 6)
#X ggplot(mod, aes(.hat, .cooksd, data = mod)) + 
#X   geom_vline(colour = NA) + 
#X   geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
#X   geom_smooth(se = FALSE) + 
#X   geom_point()
#X qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
fortify.lm <- function(model, data = model$model, ...) {
  infl <- influence(model, do.coef = FALSE)
  data$.hat <- infl$hat
  data$.sigma <- infl$sigma 
  data$.cooksd <- cooks.distance(model, infl)

  data$.fitted <- predict(model)
  data$.resid <- resid(model)
  data$.stdresid <- rstandard(model, infl)

  data
}
