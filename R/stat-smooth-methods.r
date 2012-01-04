# Prediction data frame
# Get predictions with standard errors into data frame
# 
# @keyword internal
# @alias predictdf.default
# @alias predictdf.glm
# @alias predictdf.loess
# @alias predictdf.locfit
predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

#' @S3method predictdf default
predictdf.default <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se,
    level = level, interval = if(se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "ymin", "ymax")
    data.frame(x = xseq, fit, se = pred$se)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  } 
}

#' @S3method predictdf glm
predictdf.glm <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se, 
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
    data.frame(x = xseq, y = model$family$linkinv(as.vector(pred)))
  }
}

#' @S3method predictdf loess
predictdf.loess <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se)

  if (se) {
    y = pred$fit
    ci <- pred$se.fit * qt(level / 2 + .5, pred$df)
    ymin = y - ci
    ymax = y + ci
    data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}

#' @S3method predictdf locfit
predictdf.locfit <- function(model, xseq, se, level) {
  pred <- predict(model, newdata = data.frame(x = xseq), se.fit = se)
                          
  if (se) {
    y = pred$fit
    ymin = y - pred$se.fit
    ymax = y + pred$se.fit
    data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}
