
## syntax sugar for + guides(XXX=YYY) as labs(XXX=YYY)
guides <- function(...) {
  args <- list(...)
  if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) args <- args[[1]]
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
## 1. guides_parse()
##      parse each scale and generate guide definition for all guides
##      here, one gdef for one scale
##
## 2. guides_merge()
##      merge gdefs if they are overlayed
##      number of gdefs may be less than number of scales
##
## 3. guides_gengrob()
##      generate ggrob from each gdef
##      one ggrob for one gdef
##
## 4. guides_build()
##      arrange all ggorbs

build_guides <- function(scales, layers, default_mapping, horizontal, theme) {

  ## set themes w.r.t. guides
  theme$legend.key.width <- theme$legend.key.width %|||% theme$legend.key.size
  theme$legend.key.height <- theme$legend.key.height %|||% theme$legend.key.size

  ## by default, direction of each guide depends on the position of the guide.
  theme$legend.direction <-
    theme$legend.direction %|||%
    switch(theme$legend.position, "top" =, "bottom" = "horizontal", "left" =, "right" = "vertical", "vertical")

  ## scales -> data for guides
  gdefs <- guides_parse(scales = scales, theme = theme)

  ## merge overlay guides
  gdefs <- guides_merge(gdefs)

  ## generate grob of each guides
  ggrobs <- guides_gengrob(gdefs, layers, default_mapping, theme)

  ## build up guides
  grobs <- guides_build(ggrobs, theme)

  return(grobs)
}

## parse each scale in scales and generate the definition of guide
guides_parse <- function(scales, theme) {

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
    if ((is.logical(guide) && !guide) || !scale$legend || is.null(scale_limits(scale))) next

    ## check the validity of guide.
    ## if guide is character, then find the guide object
    guide <- validate_guide(guide)

    ## title of this grob
    guide$title <- guide$title %||% scale$name %||% theme$labels[[output]]

    ## direction of this grob
    guide$direction <- guide$direction %||% theme$legend.direction

    ## each guide object parses scale within the object,
    ## so Guides (i.e., the container of guides) need not to know about them
    gdefs[[length(gdefs)+1]] <- guide_parse(guide, scale)
  }
  return(gdefs)
}

## merge overlapped guides
guides_merge <- function(gdefs) {
  ret_gdefs <- list()
  hashes <- c()
  for(gdef in gdefs) {

    ## check guides overlaying
    hash <- gdef$hash
    dest <- which(hash == hashes)

    ## logically, this never happens, so maybe need not check
    if (length(dest) > 1) {
      stop("Failed to set up guides")
    }
    ## a guide whose hash is same already exists
    ## in this case, this guide is merged to the existing guide.
    ## the way of merging depends on the type of guide
    else if (length(dest) == 1) {
      ret_gdefs[[dest]] <- guide_merge(ret_gdefs[[dest]], gdef)
    }
    ## otherwise, this guide is used
    else {
      hashes[length(hashes)+1] <- hash
      ret_gdefs[[length(ret_gdefs)+1]] <- gdef
    }
  }
  return(ret_gdefs)
}

## generate grob from each gdef (needs to write this function?)
guides_gengrob <- function(gdefs, layers, default_mapping, theme) {
  lapply(gdefs, guide_gengrob, layers, default_mapping, theme)
}

## build up all guide-grob into one guides-grob
## should be rewritten for more flexibility (?)
guides_build <- function(ggrobs, theme) {

  horizontal <- (theme$legend.box == "horizontal") %|||% (any(c("top", "bottom") %in% theme$legend.position))
  
  ## override alignment of legends box if theme$legend.box is specified
  n <- length(ggrobs)
  box_nrow <- if (horizontal) 1 else n
  box_ncol <- if (horizontal) n else 1
  ## create viewport for the guide grobs
  if (!horizontal) {
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
guide_parse <- function(...) UseMethod("guide_parse")
guide_merge <- function(...) UseMethod("guide_merge")
guide_gengrob <- function(...) UseMethod("guide_gengrob")
