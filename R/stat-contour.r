StatContour <- proto(Stat, {
  calculate <- function(., data, scales, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, name = "stat_contour")
    
    levels <- scales$get_scales("z")$domain_breaks()
    
    gridise <- function(x) {
      unique <- sort(unique(x[!is.na(x)]))
      group <- match(x, unique)
      list(unique=unique, group=group)
    }

    gridx <- gridise(data$x)
    gridy <- gridise(data$y)

    gridz <- matrix(NA, nrow = length(gridx$unique), ncol = length(gridy$unique))
    gridz[(gridy$group - 1) * length(gridx$unique) + gridx$group] <- data$z

    cl <- contourLines(x = gridx$unique, y = gridy$unique, z = gridz, levels = levels)  
     
    cl <- mapply(function(x, piece) {
      rbind(data.frame(x, piece=piece), c(NA, NA, NA))
    }, cl, seq_along(cl), SIMPLIFY=FALSE)
    do.call("rbind", cl)
  }

  objname <- "contour" 
  desc <- "Contours of 3d data"
  
  icon <- function(.) GeomContour$icon()
  
  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(group = ..piece..)
  required_aes <- c("x", "y", "z")
  desc_outputs <- list(
    level = "z value of contour"
  )
  
  examples <- function(.) {
    # Generate data
    volcano3d <- rename(melt(volcano), c(X1="x", X2="y", value="z"))
    v <- ggplot(volcano3d, aes(x=x,y=y,z=z))
    v + stat_contour()

    # Add aesthetic mappings
    v + stat_contour(aes(size = ..level..))
    v + stat_contour(aes(colour = ..level..))

    # Change scale
    v + stat_contour(aes(colour = ..level..), size=2) + 
      scale_colour_gradient(low="brown", high="white")

    v + stat_contour() + scale_z_continuous(breaks=c(100, 150))
    v + stat_contour(size=0.5) + scale_z_continuous(breaks=seq(95, 195, by=2))
    v + stat_contour() + scale_z_log10()

    # Set aesthetics to fixed value
    v + stat_contour(colour="red")
    v + stat_contour(size=2, linetype=4)

    # Try different geoms
    v + stat_contour(geom="polygon", aes(fill=..level..))
    v + geom_tile(aes(fill=z)) + stat_contour()
    
    # Use qplot instead
    qplot(x, y, z, data=volcano3d, geom="contour")
    qplot(x, y, z, data=volcano3d, stat="contour", geom="path")
  }
})

