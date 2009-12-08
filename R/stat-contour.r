StatContour <- proto(Stat, {
  calculate <- function(., data, scales, bins=NULL, binwidth=NULL, breaks = NULL, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, name = "stat_contour")

    # If no parameters set, use pretty bins
    if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
      breaks <- pretty(range(data$z), 10)
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(range(data$z)) / bins
    }
    # If necessary, compute breaks from binwidth
    if (is.null(breaks)) {
      breaks <- fullseq(range(data$z), binwidth)
    }
    
    z <- tapply(data$z, data[c("x", "y")], identity)
    cl <- contourLines(x = unique(data$x), y = unique(data$y), z = z, 
      levels = breaks)  
    cl <- lapply(cl, as.data.frame)
    
    contour_df <- rbind.fill(cl)
    contour_df$piece <- rep(seq_along(cl), sapply(cl, nrow))
    contour_df$group <- paste(data$group[1], contour_df$piece, sep = "-")
    contour_df
  }

  objname <- "contour" 
  desc <- "Contours of 3d data"
  
  icon <- function(.) GeomContour$icon()
  
  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(order = ..level..)
  required_aes <- c("x", "y", "z")
  desc_outputs <- list(
    level = "z value of contour"
  )
  
  examples <- function(.) {
    # Generate data
    volcano3d <- melt(volcano)
    names(volcano3d) <- c("x", "y", "z")

    # Basic plot
    v <- ggplot(volcano3d, aes(x, y, z = z))
    v + stat_contour()

    # Setting bins creates evenly spaced contours in the range of the data
    v + stat_contour(bins = 2)
    v + stat_contour(bins = 10)
    
    # Setting binwidth does the same thing, parameterised by the distance
    # between contours
    v + stat_contour(binwidth = 2)
    v + stat_contour(binwidth = 5)
    v + stat_contour(binwidth = 10)
    v + stat_contour(binwidth = 2, size = 0.5, colour = "grey50") +
      stat_contour(binwidth = 10, size = 1)

    # Add aesthetic mappings
    v + stat_contour(aes(size = ..level..))
    v + stat_contour(aes(colour = ..level..))

    # Change scale
    v + stat_contour(aes(colour = ..level..), size = 2) + 
      scale_colour_gradient(low = "brown", high = "white")

    # Set aesthetics to fixed value
    v + stat_contour(colour = "red")
    v + stat_contour(size = 2, linetype = 4)

    # Try different geoms
    v + stat_contour(geom="polygon", aes(fill=..level..))
    v + geom_tile(aes(fill = z)) + stat_contour()
    
    # Use qplot instead
    qplot(x, y, z, data = volcano3d, geom = "contour")
    qplot(x, y, z, data = volcano3d, stat = "contour", geom = "path")
  }
})

