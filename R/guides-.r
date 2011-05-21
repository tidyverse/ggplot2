
## syntax sugar for + guides(XXX=YYY) as labs(XXX=YYY)
guides <- function(...) {
  args <- list(...)
  if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) args <- args[[1]]
  args <- rename_aes(args)
  structure(args, class = "guides")
}

update_guides <- function(p, guides) {
  p <- plot_clone(p)
  p + opts(guides = guides)
}


## builing guides - called in ggplotGrob (plot-render.r)
##
## the procedure is as followings:
##
## 1. guides_train()
##      train each scale and generate guide definition for all guides
##      here, one gdef for one scale
##
## 2. guides_merge()
##      merge gdefs if they are overlayed
##      number of gdefs may be less than number of scales
##
## 3. guides_geom()
##      process layer information and generate geom info.
##
## 4. guides_gengrob()
##      generate ggrob from each gdef
##      one ggrob for one gdef
##
## 5. guides_build()
##      arrange all ggorbs

build_guides <- function(scales, layers, default_mapping, theme) {

  ## set themes w.r.t. guides
  ## should these theme$legend.XXX be renamed to theme$guide.XXX ?
  
  ## by default, horizontal for top and bottom, and vertical for left and right side.
  theme$legend.box <- theme$legend.box %||% if (any(c("top", "bottom") %in% theme$legend.position)) "horizontal" else "vertical"

  ## size of key (also used for bar in colorbar guide)
  theme$legend.key.width <- theme$legend.key.width %||% theme$legend.key.size
  theme$legend.key.height <- theme$legend.key.height %||% theme$legend.key.size

  ## by default, direction of each guide depends on the position of the guide.
  theme$legend.direction <-
    theme$legend.direction %||%
    switch(theme$legend.position, "top" =, "bottom" = "horizontal", "left" =, "right" = "vertical", "vertical")

  ## scales -> data for guides
  gdefs <- guides_train(scales = scales, theme = theme)
  if (length(gdefs) == 0) return(zeroGrob())

  ## merge overlay guides
  gdefs <- guides_merge(gdefs)

  ## process layer information
  gdefs <- guides_geom(gdefs, layers, default_mapping)

  ## generate grob of each guides
  ggrobs <- guides_gengrob(gdefs, theme)

  ## build up guides
  grobs <- guides_build(ggrobs, theme)

  grobs
}

## validate guide object
validate_guide <- function(guide) {
  ## if guide is specified by character, then find the corrsponding guide
  if (is.character(guide))
    match.fun(paste("guide_", guide, sep=""))()
  else if (inherits(guide, "guide"))
    guide
  else
    stop("Unknown guide: ", guide)
}

## train each scale in scales and generate the definition of guide
guides_train <- function(scales, theme) {

  gdefs <- list()
  for(scale in scales$scales) {

    ## guides(XXX) is stored in theme$guides[[XXX]],
    ## which is prior to scale_ZZZ(guide=XXX)
    ## guide is determined in order of:
    ##   + guides(XXX) > + scale_ZZZ(guide=XXX) > default(i.e., legend)
    output <- scale$aesthetics[1]
    guide <- theme$guides[[output]] %||% scale$guide 

    ## this should be changed to testing guide == "none"
    ## scale$legend is backward compatibility
    ## if guides(XXX=FALSE), then scale_ZZZ(guides=XXX) is discarded.
    if (guide=="none" || (is.logical(guide) && !guide)) next
    if (!scale$legend || is.null(scale_limits(scale))) next ## for backward compatibility

    ## check the validity of guide.
    ## if guide is character, then find the guide object
    guide <- validate_guide(guide)

    ## check the consistency of the guide and scale.
    if (guide$available_aes != "any" && ! scale$aesthetics %in% guide$available_aes)
      stop (paste("Guide '", guide$name, "' cannot be used for '", scale$aesthetics, "'.", sep=""))

    ## title of this grob
    if (is.waive(guide$title)) guide$title <- scale$name %||% theme$labels[[output]]

    ## direction of this grob
    guide$direction <- guide$direction %||% theme$legend.direction

    ## each guide object trains scale within the object,
    ## so Guides (i.e., the container of guides) need not to know about them
    guide <- guide_train(guide, scale)

    if (!is.null(guide)) gdefs[[length(gdefs)+1]] <- guide
  }
  gdefs
}

## merge overlapped guides
guides_merge <- function(gdefs) {
  ## split gdefs based on hash, and apply Reduce (guide_merge) to each gdef groug.
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}

## process layer information
guides_geom <- function(gdefs, layers, default_mapping) {
  lapply(gdefs, guide_geom, layers, default_mapping)
}

## generate grob from each gdef (needs to write this function?)
guides_gengrob <- function(gdefs, theme) {
  ## common drawing process for all guides
  gdefs <- lapply(gdefs,
    function(g) {
      g$title.position <- g$title.position %||% switch(g$direction, vertical="top", horizontal="left")
      if (!g$title.position %in% c("top", "bottom", "left", "right")) stop("title position \"", g$title.position, "\" is invalid")
      g
    })
  
  lapply(gdefs, guide_gengrob, theme)
}

## build up all guide-grob into one guides-grob
## should be rewritten for more flexibility (?)
guides_build <- function(ggrobs, theme) {

  ## override alignment of legends box if theme$legend.box is specified
  n <- length(ggrobs)
  box_nrow <- if (theme$legend.box == "horizontal") 1 else n
  box_ncol <- if (theme$legend.box == "horizontal") n else 1
  ## create viewport for the guide grobs
  if (theme$legend.box != "horizontal") {
    widths <-   do.call("max", lapply(ggrobs, function(ggrob) sum(ggrob$vp$layout$widths)))
    heights <- do.call("unit.c", lapply(ggrobs, function(ggrob) sum(ggrob$vp$layout$heights) * 1.1))
    legend.layout <- grid.layout(nrow=n, ncol=1, widths=widths, heights=heights)
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]]$vp <- viewport(layout.pos.col = 1, layout.pos.row = i, layout = ggrobs[[i]]$vp$layout)
    }
  } else {
    heights <- do.call("sum", lapply(ggrobs, function(ggrob) sum(ggrob$vp$layout$heights)))
    widths <- do.call("unit.c", lapply(ggrobs, function(ggrob) sum(ggrob$vp$layout$widths) * 1.1))
    legend.layout <- grid.layout(nrow=1, ncol=n, widths=widths, heights=heights)
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]]$vp <- viewport(layout.pos.col = i, layout.pos.row = 1, layout = ggrobs[[i]]$vp$layout)
    }
  }
  sizedGTree(children = gList(gTree(children=do.call("gList", ggrobs), vp=viewport(layout=legend.layout))), width=sum(widths), height=sum(heights))
}

## S3 dispatch
guide_train <- function(...) UseMethod("guide_train")
guide_merge <- function(...) UseMethod("guide_merge")
guide_geom <- function(...) UseMethod("guide_geom")
guide_gengrob <- function(...) UseMethod("guide_gengrob")
