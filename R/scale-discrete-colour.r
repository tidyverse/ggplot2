ScaleColour <- proto(ScaleDiscrete, expr={
  objname <- "colour"
  doc <- FALSE
  common <- c()
})

ScaleHue <- proto(ScaleColour, expr={
  aliases <- c("scale_colour_discrete", "scale_fill_discrete", "scale_color_hue")
  
  new <- function(., name=NULL, h=c(0,360) + 15, l=65, c=100, limits=NULL, breaks = NULL, labels=NULL, h.start = 0, direction = 1,  formatter = identity, legend = TRUE, variable) {
    b_and_l <- check_breaks_and_labels(breaks, labels)
    
    .$proto(name=name, h=h, l=l, c=c, .input=variable, .output=variable, .labels = b_and_l$labels, breaks = b_and_l$breaks, direction = direction, start  = h.start, limits = limits, formatter = formatter, legend = legend)
  }
  
  output_set <- function(.) {
    
    rotate <- function(x) (x + .$start) %% 360 * .$direction

    n <- length(.$input_set())
    if ((diff(.$h) %% 360) < 1) {
      .$h[2] <- .$h[2] - 360 / n
    }

    grDevices::hcl(
      h = rotate(seq(.$h[1], .$h[2], length = n)), 
      c =.$c, 
      l =.$l
    )
  }
  max_levels <- function(.) Inf

  doc <- TRUE
  common <- c("colour", "fill")

  # Documentation -----------------------------------------------
  objname <- "hue"
  desc <- "Qualitative colour scale with evenly spaced hues"
  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=hcl(seq(0, 360, length=6)[-6], c=100, l=65), col=NA)
    )
  }
  
  desc_params <- list(
    h = "range of hues to use, in [0, 360]", 
    l = "luminance (lightness), in [0, 100]",
    c = "chroma (intensity of colour)",
    h.start = "hue to start at",
    direction = "direction to travel around the colour wheel, 1 = clockwise, -1 = counter-clockwise"
  )
  
  examples <- function(.) {
    dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
    (d <- qplot(carat, price, data=dsamp, colour=clarity))
  
    # Change scale label
    d + scale_colour_hue()
    d + scale_colour_hue("clarity")
    d + scale_colour_hue(expression(clarity[beta]))
    
    # Adjust luminosity and chroma
    d + scale_colour_hue(l=40, c=30)
    d + scale_colour_hue(l=70, c=30)
    d + scale_colour_hue(l=70, c=150)
    d + scale_colour_hue(l=80, c=150)
    
    # Change range of hues used
    d + scale_colour_hue(h=c(0, 90))
    d + scale_colour_hue(h=c(90, 180))
    d + scale_colour_hue(h=c(180, 270))
    d + scale_colour_hue(h=c(270, 360))
    
    # Vary opacity
    # (only works with pdf, quartz and cairo devices)
    d <- ggplot(dsamp, aes(carat, price, colour = clarity))
    d + geom_point(alpha = 0.9)
    d + geom_point(alpha = 0.5)
    d + geom_point(alpha = 0.2)
  }
})



ScaleBrewer <- proto(ScaleColour, expr={
  doc <- TRUE

  new <- function(., name=NULL, palette=1, type="qual", limits=NULL, breaks = NULL, labels=NULL, formatter = identity, variable, legend = TRUE) {
    b_and_l <- check_breaks_and_labels(breaks, labels)
    .$proto(name=name, palette=palette, type=type, .input=variable, .output=variable, .labels = b_and_l$labels, breaks = b_and_l$breaks, limits= limits, formatter = formatter, legend = legend)
  }
  aliases <- c("scale_color_brewer")

  output_set <- function(.) {
    n <- length(.$input_set())
    RColorBrewer::brewer.pal(n, .$pal_name())[1:n]
  }

  pal_name <- function(.) {
    if (is.character(.$palette)) {
      if (!.$palette %in% RColorBrewer:::namelist) {
        warning("Unknown palette ", .$palette)
        .$palette <- "Greens"
      }
      return(.$palette)
    }
    
    switch(.$type, 
      div = RColorBrewer:::divlist, 
      qual = RColorBrewer:::quallist, 
      seq = RColorBrewer:::seqlist
    )[.$palette]
  }
  
  max_levels <- function(.) {
    RColorBrewer:::maxcolors[RColorBrewer:::namelist == .$pal_name()]
  }

  # Documentation -----------------------------------------------

  objname <- "brewer"
  desc <- "Sequential, diverging and qualitative colour scales from colorbrewer.org"
  
  desc_params <- list(
    palette = "Either numeric or character.  If numeric, selects the nth palette of type type.  If character, selects the named palette.  Get a complete list of all parameters by running \\code{RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)}",
    type = "Type of scale.  One of 'div' (diverging), 'qual' (qualitative, the default), 'seq' (sequential), or 'all' (all).  Only used when palette is numeric." 
  )
  
  details <- "<p>See <a href='http://colorbrewer.org'>colorbrewer.org</a> for more info</p>"
  common <- c("colour", "fill")

  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=RColorBrewer::brewer.pal(5, "PuOr"), col=NA)
    )
  }
  
  examples <- function(.) {
    dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
    (d <- qplot(carat, price, data=dsamp, colour=clarity))
    
    # Change scale label
    d + scale_colour_brewer()
    d + scale_colour_brewer("clarity")
    d + scale_colour_brewer(expression(clarity[beta]))

    # Select brewer palette to use, see ?brewer.pal for more details
    d + scale_colour_brewer(type="seq")
    d + scale_colour_brewer(type="seq", palette=3)

    RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)

    d + scale_colour_brewer(palette="Blues")
    d + scale_colour_brewer(palette="Set1")
    
    # scale_fill_brewer works just the same as 
    # scale_colour_brewer but for fill colours
    ggplot(diamonds, aes(x=price, fill=cut)) + 
      geom_histogram(position="dodge", binwidth=1000) + 
      scale_fill_brewer()
    
  }
})