# Set x limits
# Convenience function to set the limits of the x axis.
# 
# @argument if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
#X xlim(15, 20)
#X xlim(20, 15)
#X xlim(c(10, 20))
#X xlim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + xlim(15, 20)
xlim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    stopifnot(length(limits) == 2)
    if (limits[1] > limits[2]) {
      scale_x_reverse(limits = limits)
    } else {
      scale_x_continuous(limits = limits)
    }
  } else {
    scale_x_discrete(limits = limits)    
  }
}

# Set y limits
# Convenience function to set the limits of the y axis.
# 
# @argument if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
#X ylim(15, 20)
#X ylim(c(10, 20))
#X ylim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ylim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    stopifnot(length(limits) == 2)
    if (limits[1] > limits[2]) {
      scale_y_reverse(limits = limits)
    } else {
      scale_y_continuous(limits = limits)
    }
  } else {
    scale_y_discrete(limits = limits)    
  }
}

# Set z limits
# Convenience function to set the limits of the z axis.
# 
# @argument if numeric, will create a continuos scale, if factor or character, will create a discrete scale
# @keyword hplot
zlim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    stopifnot(length(limits) == 2)
    if (limits[1] > limits[2]) {
      scale_z_reverse(limits = limits)
    } else {
      scale_z_continuous(limits = limits)
    }
  } else {
    scale_z_discrete(limits = limits)    
  }
}
