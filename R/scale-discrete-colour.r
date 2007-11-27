ScaleColour <- proto(ScaleDiscrete, expr={
  common <- c("colour", "fill")
  objname <- "colour"
  doc <- FALSE
  guide_legend_geom <- function(.) GeomTile
})

ScaleHue <- proto(ScaleColour, expr={
  new <- function(., name=NULL, h=c(0,360), l=65, c=100, alpha=1, variable) {
    .$proto(name=name, h=h, l=l, c=c, alpha=alpha, .input=variable, .output=variable)
  }
  
  breaks <- function(.) {
    n <- length(.$domain())
    grDevices::hcl(seq(.$h[1], .$h[2], length = n+1), c=.$c, l=.$l, alpha=.$alpha)[-(n+1)]
  }
  max_levels <- function(.) Inf

  doc <- TRUE

  # Documetation -----------------------------------------------
  objname <- "hue"
  desc <- "Colours that vary continuously in hue"
  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=hcl(seq(0, 360, length=6)[-6], c=100, l=65), col=NA)
    )
  }
  
  desc_params <- list(
    h = "range of hues to use, in degrees", 
    l = "luminance",
    c = "chroma",
    alpha = "alpha"
  )
  
  examples <- function(.) {
    (d <- qplot(carat, price, data=diamonds, colour=clarity))
  
    # Change scale label
    d + scale_colour_discrete()
    d + scale_colour_discrete("clarity")
    d + scale_colour_discrete(expression(clarity[beta]))
    
    # Adjust luminosity and chroma
    d + scale_colour_discrete(l=40, c=30)
    d + scale_colour_discrete(l=70, c=30)
    d + scale_colour_discrete(l=70, c=150)
    d + scale_colour_discrete(l=80, c=150)
    
    # Change range of hues used
    d + scale_colour_discrete(h=c(0, 90))
    d + scale_colour_discrete(h=c(90, 180))
    d + scale_colour_discrete(h=c(180, 270))
    d + scale_colour_discrete(h=c(270, 360))
    
    # Vary opacity
    # (only works with pdf, quartz and cairo devices)
    d + scale_colour_discrete(alpha = 0.9)
    d + scale_colour_discrete(alpha = 0.5)
    d + scale_colour_discrete(alpha = 0.2)
  }
})
ScaleColourDiscrete <- proto(ScaleHue, objname="discrete", doc=FALSE, examples=function(.) {})


ScaleBrewer <- proto(ScaleColour, expr={
  doc <- TRUE

  new <- function(., name=NULL, palette=1, type="qual", alpha=1, reverse = FALSE, variable) {
    .$proto(name=name, palette=palette, type=type, .input=variable, .output=variable, .alpha=alpha, .reverse = reverse)
  }

  breaks <- function(.) {
    n <- length(.$domain())
    pal <- brewer.pal(n, .$pal_name())
    if (.$.reverse) pal <- rev(pal)
    alpha(pal, .$.alpha)
  }

  pal_name <- function(.) {
    if (is.character(.$palette)) {
      if (!.$palette %in% RColorBrewer:::namelist) {
        warn("Unknown palette ", .$palette)
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
  
  max_levels <- function(.) RColorBrewer:::maxcolors[RColorBrewer:::namelist == .$pal_name()]

  # Documetation -----------------------------------------------

  objname <- "brewer"
  desc <- "Colour brewer colour scales"
  details <- "<p>See <a href='http://colorbrewer.org'>colorbrewer.org</a> for more info</p>"

  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=brewer.pal(5, "PuOr"), col=NA)
    )
  }
  
  examples <- function(.) {
    (d <- qplot(carat, price, data=diamonds, colour=clarity))
    
    # Change scale label
    d + scale_colour_brewer()
    d + scale_colour_brewer("clarity")
    d + scale_colour_brewer(expression(clarity[beta]))

    # Select brewer palette to use, see ?brewer.pal for more details
    d + scale_colour_brewer(type="seq")
    d + scale_colour_brewer(type="seq", palette=3)

    display.brewer.all(n=8, exact.n=FALSE)

    d + scale_colour_brewer(palette="Blues")
    d + scale_colour_brewer(palette="Set1")
    
    # One way to deal with overplotting - use transparency
    # (only works with pdf, quartz and cairo devices)
    d + scale_colour_brewer(alpha = 0.2)
    d + scale_colour_brewer(alpha = 0.01)
  
    # scale_fill_brewer works just the same as 
    # scale_colour_brewer but for fill colours
    ggplot(diamonds, aes(x=price, fill=cut)) + 
      geom_bar(position="dodge") + 
      scale_fill_brewer()
    
  }
})