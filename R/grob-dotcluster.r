dotclusterGrob <- function (
    binaxis = "x",
    binpositions = unit(0.5, "npc"),
    bincounts = 0,
    bintotals = 0, 
    baseline = unit(0.5, "npc"),
    binwidth = unit(1, "npc"), heightratio = 1,
    stackdir = "up",
    stackratio = 1,
    dotsize =1,
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
         stackdir = stackdir, stackratio = stackratio, dotsize = dotsize, just = just,
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

  # Some conversions to absolute coordinates are needed because in npc coordinates,
  # x and y aren't necessarily square.
  # Some of these conversions are needed so we can do arithmetic on the units
  binwidthnpc    <- convert_binaxis(x$binwidth, "npc", valueOnly=TRUE)
  binwidthmm     <- convert_binaxis(x$binwidth, "mm", valueOnly=TRUE)
  dotdiamm       <- binwidthmm * x$dotsize
  dotheightnpc   <- convert_stackaxis(unit(dotdiamm, "mm"), "npc", valueOnly=TRUE)
  stackheightnpc <- dotheightnpc * x$stackratio

  baselinenpc <- convert_binaxis(x$baseline, "npc", valueOnly=TRUE)

  # Center position of the first dot in each stack, in npc coordinates
  firstdotcenternpc <- baselinenpc + (0.5-x$just) * dotheightnpc

  # Start from 0
  bincounts <- x$bincounts-1

  # Do stacking
  if (x$stackdir == "up")
    stackpos <- firstdotcenternpc + bincounts * stackheightnpc
  else if (x$stackdir == "down")
    stackpos <- firstdotcenternpc - bincounts * stackheightnpc
  else if (x$stackdir == "center")
    stackpos <- firstdotcenternpc + (bincounts+0.5-(x$bintotals/2)) * stackheightnpc
  else if (x$stackdir == "centerwhole")
    stackpos <- firstdotcenternpc + ceiling(bincounts+0.5-(x$bintotals/2)) * stackheightnpc
  else if (x$stackdir == "centerwholedown")
    stackpos <- firstdotcenternpc + floor  (bincounts+0.5-(x$bintotals/2)) * stackheightnpc

  
  if(x$binaxis == "x") {
    xpos <- x$binpositions
    ypos <- stackpos
  } else if(x$binaxis == "y") {
    xpos <- stackpos
    ypos <- x$binpositions
  }

  grid.draw(
    circleGrob(x=xpos, y=ypos,
               r=unit(dotdiamm/2, "mm"),   # Need absolute measurement because if you use npc coordinates, r is relative to the smaller direction of x and y
               name=x$name, gp=x$gp, vp=x$vp),
  )
}
