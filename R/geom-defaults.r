# Update geom defaults
# Modify geom aesthetic defaults for future plots
# 
# @keywords name of geom to modify
# @keywords named list of aesthetics
# @keywords hplot
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