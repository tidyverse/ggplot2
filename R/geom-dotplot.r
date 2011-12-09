#' Dot plot
#' 
geom_dotplot <- function (mapping = NULL, data = NULL, stat = "bindot", position = "identity",
na.rm = FALSE, binaxis = "x", binstataxis = "x", binmethod="dotdensity", stackdir = "up",
stackratio = 1, dotsize = 1, ...) {
  GeomDotplot$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, binaxis = binaxis, binstataxis = binstataxis,
  binmethod = binmethod, stackdir = stackdir, stackratio = stackratio, dotsize = dotsize, ...)
}

# TODO:
# Get rid of binstataxis parameter - use only binaxis - how do you get that parameter to stat and Geom$draw?
# Option to vertically align points on grid - do without stretching
# Legend appearance
# Icon

GeomDotplot <- proto(Geom, {
  objname <- "dotplot"

# Is draw_groups needed?
#  draw_groups <- function(., ...) .$draw(...)

  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    # Set up the stacking function
    if(params$stackdir=="up")
      stackdots <- function(a) return(a-.5)
    else if (params$stackdir=="down")
      stackdots <- function(a) return(-a+.5)
    else if (params$stackdir=="center")
      stackdots <- function(a) return(a-1-max(a-1)/2)
    else if (params$stackdir=="centerwhole")
      stackdots <- function(a) return(a-1-floor(max(a-1)/2))
    else if (params$stackdir=="centerwholedown")
      stackdots <- function(a) return(a-1-ceiling(max(a-1)/2))

    if (params$binaxis=="x") {
      # Fill the bins: at a given x, if count=3, make 3 entries at that x, with y=1,2,3
      df <- ddply(df, .(x, group), function(xx) {
                      if(xx$count==0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx$y <- stackdots(xx$countidx)
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      # Can't do bounding box per dot, because it doesn't play well with pos_dodge.
      # In the future, each dot should have its own bounding rectangle.
      # xmin and xmax aren't really the x bounds, because of the odd way the grob
      # works. They're set to the max count value so that it's possible to align
      # the dots with the y axis.
      df <- ddply(df, .(group), transform,
            ymin = min(y)-.5,
            ymax = max(y)+.5,
            xmin = min(x) - binwidth[1]/2,
            xmax = max(x) + binwidth[1]/2)

    } else if (params$binaxis=="y") {
      df <- ddply(df, .(y, group), function(xx) {
                      if(xx$count==0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx$xoffset <- stackdots(xx$countidx)
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      # xmin and xmax aren't really the x bounds, because of the odd way the grob
      # works. They're just set to the standard x +- width/2 so that dot clusters
      # can be dodged like other geoms.
      # In the future, each dot should have its own bounding rectangle.
      df <- ddply(df, .(group), transform,
            ymin = min(y) - binwidth[1]/2,
            ymax = max(y) + binwidth[1]/2)

      # For y binning,, it is impossible to stack with each dot at 1 greater x.
      # I don't really know why.
      # Instead, we'll scale the dot clusters to fit within the specified width.
      df$xmin <- df$x - df$width / 2
      df$xmax <- df$x + df$width / 2
    }
    df
  }
  

  draw <- function(., data, scales, coordinates, na.rm = FALSE, binaxis = "x",
                   stackdir = "up", stackratio = 1, dotsize = 1, ...) {
    data <- remove_missing(data, na.rm, 
      c("x", "y", "size", "shape"), name = "geom_dotplot")
    if (empty(data)) return(zeroGrob())


    # Transform the data to the new coordinates
    tdata <- coord_transform(coordinates, data, scales)


    # Is there a better way of generalizing over x and y?
    if (binaxis=="x") {
      dotwidthnpc  <- tdata$binwidth[1] / (max(scales$x.range) - min(scales$x.range))

      # A little hack-y way to get the x=0 and y=0 in npc coordinates
      zeronpc <- coord_transform(coordinates, data.frame(y=0), scales)
      stackbaselinenpc <- zeronpc$y
      binpositions <- tdata$x
      stackpositions <- data$y

    } else if (binaxis=="y") {
      dotwidthnpc  <- tdata$binwidth[1] / (max(scales$y.range) - min(scales$y.range))
      stackbaselinenpc <- tdata$x
      binpositions <- tdata$y
      # This is handled differently from y because x can be grouped in factors
      stackpositions <- data$xoffset
    }


    ggname(.$my_name(),
      grobTree(
        dotclusterGrob(binaxis, binpositions, stackpos=stackpositions, bintotals=tdata$count,
                baseline=stackbaselinenpc, binwidth=dotwidthnpc,
                stackdir=stackdir, stackratio=stackratio, dotsize=dotsize,
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
  default_aes <- function(.) aes(y=..count.., shape=16, colour="black", size=2, fill = "black", alpha = 1)
  
})
