# TODO: make sure params are correct units, etc.
dotclusterGrob <- function (
    binaxis = "x",
    binpositions = unit(0.5, "npc"),
    stackpos = 0,
    bintotals = 0, 
    baseline = unit(0.5, "npc"),
    binwidth = unit(1, "npc"),
    stackdir = "up",
    stackratio = 1,
    dotsize =1,
    default.units = "npc", name = NULL, gp = gpar(), 
    vp = NULL) 
{
    if (!is.unit(binpositions)) 
        x <- unit(binpositions, default.units)
    if (!is.unit(baseline)) 
        baseline <- unit(baseline, default.units)
    if (!is.unit(binwidth)) 
        binwidth <- unit(binwidth, default.units)

    grob(binaxis = binaxis, binpositions = binpositions, stackpos = stackpos, bintotals = bintotals, 
         baseline = baseline, binwidth = binwidth,
         stackdir = stackdir, stackratio = stackratio, dotsize = dotsize,
         name = name, gp = gp, vp = vp, cl = "dotclustergrob")
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
  firstdotcenternpc <- baselinenpc

  # Start from 0
  bincounts <- x$bincounts-1


  if(x$binaxis == "x") {
    xpos <- x$binpositions
    ypos <- firstdotcenternpc + x$stackpos * dotheightnpc * x$stackratio
  } else if(x$binaxis == "y") {
    xpos <- firstdotcenternpc + x$stackpos * dotheightnpc * x$stackratio
    ypos <- x$binpositions
  }

  grid.draw(
    circleGrob(x=xpos, y=ypos,
               r=unit(dotdiamm/2, "mm"),   # Need absolute measurement because if you use npc coordinates, r is relative to the smaller direction of x and y
               name=x$name, gp=x$gp, vp=x$vp),
  )
}
