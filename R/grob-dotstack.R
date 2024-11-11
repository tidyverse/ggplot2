dotstackGrob <- function(
    x = unit(0.5, "npc"),     # x pos of the dotstack's origin
    y = unit(0.5, "npc"),     # y pos of the dotstack's origin
    stackaxis = "y",
    dotdia = unit(1, "npc"),  # Dot diameter in the non-stack axis, should be in npc
    stackposition = 0,        # Position of each dot in the stack, relative to origin
    stackdir = "up",          # Stacking direction ("up", "down", "center", or "centerwhole")
    stackratio = 1,           # Stacking height of dots (.75 means 25% dot overlap)
    default.units = "npc", name = NULL, gp = gpar(), vp = NULL)
{
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(dotdia))
        dotdia <- unit(dotdia, default.units)
    if (!unitType(dotdia) == "npc")
        cli::cli_warn("Unit type of dotdia should be {.val npc}")

    grob(x = x, y = y, stackaxis = stackaxis, dotdia = dotdia,
         stackposition = stackposition, stackdir = stackdir, stackratio = stackratio,
         name = name, gp = gp, vp = vp, cl = "dotstackGrob")
}

#' @export
makeContext.dotstackGrob <- function(x) {
  # Need absolute coordinates because when using npc coords with circleGrob,
  # the radius is in the _smaller_ of the two axes. We need the radius
  # to instead be defined in terms of the non-stack axis.
  xmm <- convertX(x$x, "mm", valueOnly = TRUE)
  ymm <- convertY(x$y, "mm", valueOnly = TRUE)

  # When stacking up (or down), stackratios != 1 will cause the bottom (top)
  # edge of the first dot in a stack to no longer touch the origin, as
  # stackpositions are expanded or contracted away from the dotstack's origin.
  # The stackoffset corrects that misalignment so that the first dot just
  # touches the dotstack's origin.
  if (is.null(x$stackdir) || x$stackdir == "up") {
    stackoffset <- (1 - x$stackratio) / 2
  } else if (x$stackdir == "down") {
    stackoffset <- -(1 - x$stackratio) / 2
  } else {
    stackoffset <- 0
  }

  if (x$stackaxis == "x") {
    dotdiamm <- convertY(x$dotdia, "mm", valueOnly = TRUE)
    xpos <- xmm + dotdiamm * (x$stackposition * x$stackratio + stackoffset)
    ypos <- ymm
  } else if (x$stackaxis == "y") {
    dotdiamm <- convertX(x$dotdia, "mm", valueOnly = TRUE)
    xpos <- xmm
    ypos <- ymm + dotdiamm * (x$stackposition * x$stackratio + stackoffset)
  }

  circleGrob(
    x = xpos, y = ypos, r = dotdiamm / 2, default.units = "mm",
    name = x$name, gp = x$gp, vp = x$vp
  )
}
