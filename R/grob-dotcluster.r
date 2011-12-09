dotclusterGrob <- function (
    binaxis = "x",
    binpositions = unit(0.5, "npc"),
    bincounts = 0,
    bintotals = 0, 
    baseline = unit(0.5, "npc"),
    binwidth = unit(1, "npc"), heightratio = 1,
    stackdir = "up",
    just = NULL, default.units = "npc", name = NULL, gp = gpar(), 
    vp = NULL) 
{
    if (!is.unit(binpositions)) 
        x <- unit(binpositions, default.units)
    if (!is.unit(baseline)) 
        baseline <- unit(baseline, default.units)
    if (!is.unit(binwidth)) 
        binwidth <- unit(binwidth, default.units)

    grob(binaxis = binaxis, binpositions = binpositions, bincounts = bincounts, bintotals = bintotals, 
         baseline = baseline,
         binwidth = binwidth, heightratio = heightratio,
         stackdir = stackdir, just = just, 
         name = name, gp = gp, vp = vp, 
         cl = "dotclustergrob")
}

drawDetails.dotclustergrob <- function(x, recording=TRUE) {

  # There's a binning axis and a stacking axis. If the binning axis is x, then the
  # stacking axis is y, and vice versa.
  if(x$binaxis == "x") {
    convert_binaxis   <- convertX
    convert_stackaxis <- convertY
  } else if (x$binaxis == "y") {
    convert_binaxis   <- convertY
    convert_stackaxis <- convertX
  }

  # Some of these conversions are necessary to do arithmetic on the units
  binwidthmm   <- convert_binaxis(x$binwidth, "mm", valueOnly=TRUE)
  binwidthnpc  <- convert_binaxis(x$binwidth, "npc", valueOnly=TRUE)
  binheightmm  <- binwidthmm
  binheightnpc <- convert_stackaxis(unit(binheightmm,"mm"), "npc", valueOnly=TRUE)

  baselinenpc <- convert_binaxis(x$baseline, "npc", valueOnly=TRUE)

  # Do stacking
  if (x$stackdir == "up")
    stackpos <-  (x$bincounts-0.5-x$just) * binheightnpc + baselinenpc
  else if (x$stackdir == "down")
    stackpos <- (-x$bincounts+1.5-x$just) * binheightnpc + baselinenpc
  else if (x$stackdir == "center")
    stackpos <- (x$bincounts-(x$bintotals/2)-x$just) * binheightnpc + baselinenpc
  else if (x$stackdir == "centerwhole")
    stackpos <- ceiling((x$bincounts-(x$bintotals/2)-x$just)) * binheightnpc + baselinenpc
  else if (x$stackdir == "centerwholedown")
    stackpos <- floor  ((x$bincounts-(x$bintotals/2)-x$just)) * binheightnpc + baselinenpc

  
  if(x$binaxis == "x") {
    xpos <- x$binpositions
    ypos <- stackpos
  } else if(x$binaxis == "y") {
    xpos <- stackpos
    ypos <- x$binpositions
  }

  grid.draw(
    circleGrob(x=xpos, y=ypos,
               r=unit(binwidthmm/2, "mm"),   # Need absolute measurement because if you use npc coordinates, r is relative to the smaller direction of x and y
               name=x$name, gp=x$gp, vp=x$vp),
  )
}
