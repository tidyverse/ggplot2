# Grob axis
# Grob for axes
# 
# @arguments position of ticks
# @arguments labels at ticks
# @arguments position of axis (top, bottom, left or right)
# @arguments range of data values
# @keyword hplot 
# @keyword internal
guide_axis <- function(at, labels, position="right", scale=c(0,1), theme) {
  #assert.equal(length(at), length(labels))
  
  positions <- c("top","bottom", "right","left")
  #position <- match(positions, position)
  
  line_grob <- guide_axis_line(at, position)
  ticks_grob <- guide_axis_ticks(at, position)
  labels_grob <- guide_axis_labels(at, labels, position)
  
  ggname("axis", gTree(
    childrenvp = guide_axis_vp(position, labels, scale), 
    children = gList(ticks_grob, labels_grob)
  ))
}
 
# Axis viewport path
# Compute viewport path for specified component of axis
# 
# @arguments position of axis
# @arguments component name
# @keyword hplot 
# @keyword internal
axis_vp_path <- function(position, name) {
  vpPath(paste(position, "axis", sep="_"), name)
}

# Grob axis line
# Grob for axis baseline
# 
# @arguments position of ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
guide_axis_line <- function(at, position) {
  vp <- axis_vp_path(position, "ticks")
  ends <- unit(range(at), "native")
  
  ggname("major", switch(position,
    top =    linesGrob(ends, unit(c(0,0), "npc"), vp=vp),
    bottom = linesGrob(ends, unit(c(1,1), "npc"), vp=vp),
    left =   linesGrob(unit(c(1,1), "npc"), ends, vp=vp),
    right =  linesGrob(unit(c(0,0), "npc"), ends, vp=vp)
  ))
  
}

# Grob axis ticks
# Grob for axis ticks
# 
# @arguments position of ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
guide_axis_ticks <- function(at, position) {
  vp <- axis_vp_path(position, "ticks")
  ggname("ticks", switch(position,
    top =    ,
    bottom = segmentsGrob(unit(at, "native"), unit(0.1, "npc"), unit(at, "native"), unit(1, "npc"), vp=vp),
    left =   ,
    right =  segmentsGrob(unit(0.2, "npc"), unit(at, "native"), unit(1, "npc"), unit(at, "native"), vp=vp),
  ))
}

# Grob axis labels
# Grob for axis lables
# 
# @arguments position of ticks
# @arguments grob labels at ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
guide_axis_labels <- function(at, labels, position) {
  vp <- axis_vp_path(position, "labels")
  gp <- gpar(cex=0.9, lineheight=0.9)
  
  ggname("labels", switch(position,
    top = ,
    bottom = gTree(children=do.call("gList", lapply(1:length(labels), function(i) {
      ggname("label", textGrob(labels[[i]], unit(at[i], "native"), unit(0.5, "npc"), just = c("centre","centre")))
    })), vp=vp, gp=gp),
    right = ,
    left = gTree(children=do.call("gList", lapply(1:length(labels), function(i) {
      ggname("label", textGrob(labels[[i]], unit(1, "npc"), unit(at[i], "native"), just = c("right","centre")))
    })), vp=vp, gp=gp)
  ))
  
  # switch(position,
  #   top =    textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
  #   bottom = textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
  #   left =   textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels"),
  #   right =  textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, gp=gp, name="axis-labels")
  # )  
}

# Grob axis viewport
# Generate viewport for axis grobs
# 
# @arguments position of axis (top, bottom, left or right)
# @arguments labels at ticks
# @arguments range of data values
# @returns viewport tree
# @keyword hplot 
# @keyword internal
guide_axis_vp <- function(position, labels, scale=c(0,1)) {
  tick_size <- unit(0.4, "lines")

  label_size <- switch(position, 
    top = ,
    bottom = max(stringHeight(labels)),
    left = ,
    right = max(stringWidth(labels))
  )
  
  #viewport with named parts labels and text
  layout <- switch(position,
    top =    grid.layout(nrow=2, ncol=1, heights=unit.c(label_size, tick_size), widths=unit(1,"npc")),
    bottom = grid.layout(nrow=2, ncol=1, heights=unit.c(tick_size, label_size), widths=unit(1,"npc")),
    left =   grid.layout(nrow=1, ncol=2, widths=unit.c(label_size, tick_size), heights=unit(1,"npc")),
    right =  grid.layout(nrow=1, ncol=2, widths=unit.c(tick_size, label_size), heights=unit(1,"npc"))
  )

  vp_top <- switch(position,
    top =    ,
    bottom = viewport(layout=layout, height=label_size + tick_size, width=unit(1,"npc"), name=paste(position, "axis", sep="_")),
    left =   ,
    right =  viewport(layout=layout, width=label_size + tick_size, height=unit(1,"npc"), name=paste(position, "axis", sep="_"))
  )
  
  vp_labels <- switch(position,
    top =    viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", xscale=scale, clip="off"),
    bottom = viewport(layout.pos.row = 2, layout.pos.col = 1, name="labels", xscale=scale, clip="off"),
    left =   viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", yscale=scale, clip="off"),
    right =  viewport(layout.pos.row = 1, layout.pos.col = 2, name="labels", yscale=scale, clip="off")
  )

  vp_ticks <- switch(position,
    top =    viewport(layout.pos.row = 2, layout.pos.col = 1, name="ticks", xscale=scale),
    bottom = viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", xscale=scale),
    left =   viewport(layout.pos.row = 1, layout.pos.col = 2, name="ticks", yscale=scale),
    right =  viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", yscale=scale)
  )
  
  vpTree(vp_top, vpList(vp_labels, vp_ticks))
  
}