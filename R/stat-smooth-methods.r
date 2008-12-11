# Prediction data frame
# Get predictions with standard errors into data frame
# 
# @keywords internal
# @alias predictdf.default
# @alias predictdf.glm
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

predictdf.default <- function(model, xseq, se, level) {
  pred <- stats::predict(model, data.frame(x = xseq), se = se)

  if (se) {
    std <- qnorm(level / 2 + 0.5)
    data.frame(
      x = xseq, y = as.vector(pred$fit),
      ymin = as.vector(pred$fit - std * pred$se), 
      ymax = as.vector(pred$fit + std * pred$se),
      se = as.vector(pred$se)
    )
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  } 
}

predictdf.glm <- function(model, xseq, se, level) {
  pred <- stats::predict(model, data.frame(x = xseq), se = se, 
    type = "link")
  
  if (se) {
    std <- qnorm(level / 2 + 0.5)
    data.frame(
      x = xseq, 
      y = model$family$linkinv(as.vector(pred$fit)),
      ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se)), 
      ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se)), 
      se = as.vector(pred$se)
    )
  } else {
    data.frame(x = xseq, y = model$family$linkinv(pred))
  }
  
}