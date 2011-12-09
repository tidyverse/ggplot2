#' Dot plot
#' 
geom_dotplot <- function (mapping = NULL, data = NULL, stat = "bindot", position = "identity",
na.rm = FALSE, just = 0, binaxis = "x", binstataxis = "x", stackdir = "up", ...) {
  GeomDotplot$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, just = just, binaxis = binaxis, binstataxis = binstataxis, stackdir = stackdir, ...)
}

# TODO:
# Get rid of binstataxis parameter - use only binaxis
# Vertically align points either on grid, or physically touch
# Bin overlap
# Stack overlap
# Add dot density algorithm
# xmin, xmax bounding box
# Set y range without using coord
# Better way of setting baseline
# Legends when used with violin

GeomDotplot <- proto(Geom, {
  objname <- "dotplot"

# Is draw_groups needed?
#  draw_groups <- function(., ...) .$draw(...)

  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    if (params$binaxis=="x") {
      # Fill the bins: at a given x, if count=3, make 3 entries at that x, with y=1,2,3
      df <- ddply(df, .(x, group), function(xx) {
                      if(xx$count==0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      # But not really.
      # This is not the same as for y! fix me!
      df <- ddply(df, .(group), transform,
            ymin = min(y),
            ymax = max(y),
            xmin = min(x),
            xmax = max(x))

    } else if (params$binaxis=="y") {
      df <- ddply(df, .(y, group), function(xx) {
                      if(xx$count==0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      # But not really.
      df <- ddply(df, .(group), transform,
            ymin = min(y),
            ymax = max(y),
            xmin = x - width / 2,
            xmax = x + width / 2)
    }

    df
  }
  

  draw <- function(., data, scales, coordinates, na.rm = FALSE, just = 0, binaxis = "x", stackdir = "up", ...) {
    data <- remove_missing(data, na.rm, 
      c("x", "y", "size", "shape"), name = "geom_dotplot")
    if (empty(data)) return(zeroGrob())


    # Transform the data to the new coordinates
    tdata <- coord_transform(coordinates, data, scales)


    # Is there a better way of generalizing over x and y?
    if (binaxis=="x") {
      dotwidthnpc  <- tdata$binwidth[1] / (max(scales$x.range) - min(scales$x.range))
      # This isn't necessarily a reliable way to get the baseline...
      stackbaselinenpc <- min(tdata$y)
      
      binpositions <- tdata$x

    } else if (binaxis=="y") {

      dotwidthnpc  <- tdata$binwidth[1] / (max(scales$y.range) - min(scales$y.range))
      stackbaselinenpc <- min(tdata$x)
      binpositions <- tdata$y
    }

    ggname(.$my_name(),
      grobTree(
        dotclusterGrob(binaxis, binpositions, bincounts=tdata$countidx, bintotals=tdata$count,
                baseline=stackbaselinenpc, 
                binwidth=dotwidthnpc, heightratio=1,
                stackdir=stackdir,
                just=just,
                 default.units="npc",
                 gp=gpar(col=alpha(tdata$colour, tdata$alpha),
                         fill=alpha(tdata$fill, tdata$alpha))))
    )

  }

  draw_legend <- function(., data, ...) {
    # If fill is set, ensure that you can actually see it
    if (!is.null(data$fill) && !all(is.na(data$fill)) && data$shape == 16) {
      data$shape <- 21
    } 
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
        gp=gpar(
          col=alpha(colour, alpha), 
          fill=alpha(fill, alpha), 
          fontsize = size * .pt)
      )
    )
  }

  icon <- function(.) {
    pos <- seq(0.1, 0.9, length=6)
    pointsGrob(x=pos, y=pos, pch=19, gp=gpar(col="black", cex=0.5), default.units="npc")
  }
  
  default_stat <- function(.) StatBindot
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(y=..count.., shape=16, colour=NA, size=2, fill = "black", alpha = 1)
  
})
