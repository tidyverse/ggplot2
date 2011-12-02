# Update geom defaults
# Modify geom aesthetic defaults for future plots
# 
# @param name of geom to modify
# @param named list of aesthetics
# @keyword hplot
#X update_geom_defaults("point", list(colour = "darkblue"))
#X qplot(mpg, wt, data = mtcars)
#X update_geom_defaults("point", list(colour = "black"))
update_geom_defaults <- function(geom, new) {
  g <- Geom$find(geom)
  old <- g$default_aes()
  
  aes <- defaults(new, old)
  
  g$default_aes <- eval(substitute(function(.) aes, list(aes = aes)))
}

# change geom aesthetics
# change geom defaults for other params
# change scale defaults
# change default scale for given aesthetic

# Update geom defaults
# Modify geom aesthetic defaults for future plots
# 
# @param name of geom to modify
# @param named list of aesthetics
# @keyword hplot
update_stat_defaults <- function(geom, new) {
  g <- Stat$find(geom)
  old <- g$default_aes()
  
  aes <- defaults(new, old)
  g$default_aes <- eval(substitute(function(.) aes, list(aes = aes)))
}
