# Fortify a model with data
# Generic method to supplement the original data with model fit statistics
# 
# @seealso \code{\link{fortify.lm}}
fortify <- function(model, data, ...) UseMethod("fortify")

# Fortify a linear model with its data
# Supplement the data fitted to a linear model with model fit statistics.
# 
# The following statistics will be added to the data frame:
# 
# \itemize{
#   \item[.hat] Diagonal of the hat matrix
#   \item[.sigma] Estimate of residual standard deviation when corresponding
#      observation is dropped from model
#   \item{.cooksd} Cooks distance, \code{\link{cooks.distance}}
#   \item{.fitted} Fitted values of model
#   \item{.resid} Residual
#  \item{.stdresid} Standardised residuals
# }
# 
# If you have missing values in your model data, you may need to refit 
# the model with \code{na.action = na.preserve}.
#
# @arguments 
#X mod <- lm(mpg ~ wt, data = mtcars)
#X fortify(mod)
#X fortify(mod, data)
#X 
#X plot(mod, which = 1)
#X qplot(.fitted, .resid, data = mod) + geom_hline() + geom_smooth(se = F)
#X qplot(.fitted, .stdresid, data = mod) + geom_hline() + geom_smooth(se = F)
#X
#X plot(mod, which = 2)
#X qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline()
#X
#X plot(mod, which = 3)
#X qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = F)
#X
#X plot(mod, which = 4)
#X qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar",
#X  stat="identity")
#X
#X plot(mod, which = 5)
#X qplot(.hat, .stdresid, data = mod) + geom_smooth(se = F)
#X ggplot(mod, aes(.hat, .stdresid)) + 
#X   geom_vline(size = 2, colour = "white") +
#X   geom_hline(size = 2, colour = "white") +
#X   geom_point() + geom_smooth(se = F)
#X 
#X qplot(.hat, .stdresid, data = mod, size = .cooksd) + 
#X   geom_smooth(se = F, size = 0.5)
#X
#X plot(mod, which = 6)
#X qplot(.hat, .cooksd, data = mod) + geom_smooth(se = F) + 
#X   geom_vline(colour = NA) + 
#X   geom_abline(slope = seq(0, 3, by = 0.5), colour = "white")
#X qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
fortify.lm <- function(model, data = model$model, ...) {
  infl <- influence(model, do.coef = FALSE)
  s <- sqrt(deviance(model) / df.residual(model))
  
  w <- weights(model) %||% 1

  data$.hat <- infl$hat
  data$.sigma <- infl$sigma 
  data$.cooksd <- cooks.distance(mod)

  data$.fitted <- predict(model)
  data$.resid <- resid(model)
  data$.stdresid <- sqrt(w) * data$.resid / (s * sqrt(1 - data$.hat))    
    
  data
}
