# Set x limits
# Convenience function to set the limits of the x axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
#X xlim(15, 20)
#X xlim(c(10, 20))
#X xlim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + xlim(15, 20)
xlim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    scale_x_continuous(limits = limits)
  } else {
    scale_x_discrete(limits = limits)    
  }
}

# Set y limits
# Convenience function to set the limits of the y axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
#X ylim(15, 20)
#X ylim(c(10, 20))
#X ylim("a", "b", "c") 
#X qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ylim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    scale_y_continuous(limits = limits)
  } else {
    scale_y_discrete(limits = limits)    
  }
}

# Set z limits
# Convenience function to set the limits of the z axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
zlim <- function(...) {
  limits <- c(...)
  if (is.numeric(limits)) {
    scale_z_continuous(limits = limits)
  } else {
    scale_z_discrete(limits = limits)    
  }
}
