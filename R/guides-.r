## classes w.r.t. guides

# encupsles all guides but positions (x, y, date)
Guides <- setRefClass("Guides",
  fields = list(
    hashes = "character", # hash list as char vector
    guides = "list", # list of guides. 
    scales = "Scales",
    layers = "list",
    default_mapping = "ANY",
    horizontal = "logical",
    theme = "list"),
                      
  methods = list(

    # arrange all guides
    build_guides = function() {

      # override alignment of legends box if theme$legend.box is specified
      if (!is.na(theme$legend.box)) {
        horizontal <<- 1 == charmatch(theme$legend.box, c("horizontal","vertical"))
      }

      #
      # parse scale and generate guide objects
      #
      
      # note that number of guides can be less than that of scales because of
      # 1) no guide scale
      # 2) common guide for multiple scale
      
      parse_scales()
      n <- length(guides)
      if (length(guides) == 0) return(zeroGrob()) # return zero grob if no guide.

      #
      # arrange the guides
      #

      # now it would be possible to specify position/direction for each guide,
      # by guide_XXX(pos="bottom/top/left/right", dir="horizontal/vertical"), not yet implemented.
      # in that case, arrange them by rewrite the following codes.
      #
      # now, all guides are placed on a same space and in sa same direction,
      # which is specified in opts(legend.direction, legend.position)
      
      box_nrow <- if (horizontal) 1 else n
      box_ncol <- if (horizontal) n else 1

      # guide -> grob of guide
      legs <- lapply(guides, function(g)g$buildGuide(layers, default_mapping, theme))

      # create viewport for the guide grobs
      if (!horizontal) {
        widths <-   do.call("max", lapply(legs, function(leg) sum(leg$vp$layout$widths)))
        heights <- do.call("unit.c", lapply(legs, function(leg) sum(leg$vp$layout$heights) * 1.1))
        legend.layout <- grid.layout(nrow=n, ncol=1, widths=widths, heights=heights)
        for (i in seq_along(legs)) {
          legs[[i]]$vp <- viewport(layout.pos.col = 1, layout.pos.row = i, layout = legs[[i]]$vp$layout)
        }
      } else {
        heights <- do.call("sum", lapply(legs, function(leg) sum(leg$vp$layout$heights)))
        widths <- do.call("unit.c", lapply(legs, function(leg) sum(leg$vp$layout$widths) * 1.1))
        legend.layout <- grid.layout(nrow=1, ncol=n, widths=widths, heights=heights)
        for (i in seq_along(legs)) {
          legs[[i]]$vp <- viewport(layout.pos.col = i, layout.pos.row = 1, layout = legs[[i]]$vp$layout)
        }
      }

      #
      # return the all guides
      #
      
      # somewhat wired trick here...
      # maybe this can be improved. 
      sizedGTree(children = gList(gTree(children=do.call("gList", legs), vp=viewport(layout=legend.layout))), width=sum(widths), height=sum(heights))
    },

    # extract information from scale and generate guide object
    parse_scales = function() {
      for(scale in scales$scales) {

        # this should be changed to test guide == "none"
        if (!scale$legend || is.null(scale_limits(scale))) next

        # this should be implemented in more general way
        if (is.character(scale$guide)) {
          guide_name <- paste("guide_", scale$guide, sep="")
          if (!exists(guide_name)) stop("No guide called ", guide_name, call.=FALSE)
          else scale$guide <- eval(call(guide_name))
        }

        scale$guide$parseScale(scale = scale, theme = theme)
        guide_hash <- scale$guide$hash()

        guide_dest <- which(guide_hash== hashes)

        if (length(guide_dest) > 1) {
          stop("Failed to set up guides")
        }
        else if (length(guide_dest) == 1) {
          guides[[guide_dest]]$mergeGuide(scale$guide)
        } else {
          hashes[length(hashes)+1] <<- guide_hash
          guides[[length(guides)+1]] <<- scale$guide
        }
      }
    }
    )
  )

# guide
#
# name
# scale -> guide
# hash generator
# guide builder

# abstract class for a guide
Guide <- setRefClass("Guide",
  fields = list(
    name = "character",
    title = "ANY", # character or expression
    key = "ANY"
    ),

  methods = list(
    parseScale = function(...) stop(""),
    hash = function() digest(list(title, key$.label, name)),
    mergeGuide = function(new_guide) {stop("")},
    buildGuide = function(...) stop("")
    )
)

GuideLegend <- setRefClass("GuideLegend",
  contains = c("Guide"),
                           
  methods = list(
    
    parseScale = function(..., scale, theme) {
      if (!scale$legend || is.null(scale_limits(scale))) return(NULL)

      output <- scale$aesthetics[1]

      .title <- scale$name %||% theme$labels[[output]]
      .key <- data.frame(
        scale_map(scale, scale_breaks(scale)), I(scale_labels(scale)), 
        stringsAsFactors = FALSE)
      names(.key) <- c(output, ".label")
      
      title <<- .title
      key <<- .key
    },
    
    mergeGuide = function(new_guide) {
      key <<- merge(key, new_guide$key)
    },
    
    buildGuide = function(layers, default_mapping, theme) {

      # grid::unit calculation is so slow
      #
      # here all units but originally "mm" are converted into "mm" first,
      # and all caulucation is done for raw numeric.
      # this save the time up to approx. 20-30%.
      # but also, conversion takes some time.
      # it is better to develop light-weight fast unit system.

      legend_data <- llply(layers, .self$build_legend_data, default_mapping)

      # Determine key width and height
      if (is.na(theme$legend.key.width)) {
        theme$legend.key.width <- theme$legend.key.size
      }
      key.width <- c(convertWidth(theme$legend.key.width, "mm"))
      
      if (is.na(theme$legend.key.height))
        theme$legend.key.height <- theme$legend.key.size
      key.height <- c(convertHeight(theme$legend.key.height, "mm"))

      ## Calculate sizes for keys - mainly for v. large points and lines
      size_mat <- do.call("cbind", llply(legend_data, "[[", "size"))
      if (is.null(size_mat)) {
        key_sizes <- rep(0, nrow(key))
      } else {
        key_sizes <- apply(size_mat, 1, max)
      }

      ## Determine the direction of the elements of legend.
      if (theme$legend.direction == "horizontal") {
        direction <- "horizontal"
      } else {
        direction <- "vertical"
      }

      ## gap between keys etc
      hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
      vgap <- hgap

      ## hjust for title of legend
      ## if direction is vertical, then title is left-aligned
      ## if direction is horizontal, then title is centre-aligned
      ## if legend.title.align is specified, then title is alinged using the value
      if (is.na(theme$legend.title.align)) {
        if (direction == "vertical") {
          grob.title <- theme_render(theme, "legend.title", title, x = 0, y = 0.5)
        } else if (direction == "horizontal") {
          grob.title <- theme_render(theme, "legend.title", title, hjust = 0.5, x = 0.5, y = 0.5)
        }
      } else {
        grob.title <- theme_render(
          theme, "legend.title", title,
          hjust = theme$legend.title.align,
          x = theme$legend.title.align, y = 0.5
        )                            
      }

      ## Compute heights and widths of legend table
      title_width <- c(convertWidth(grobWidth(grob.title), "mm"))
      title_height <- c(convertHeight(grobHeight(grob.title), "mm"))

      ## text label alignment
      if (is.na(theme$legend.text.align)) {
        numeric_labels <- all(sapply(key$.label, is.language)) || suppressWarnings(all(!is.na(sapply(key$.label, "as.numeric"))))
        hpos <- numeric_labels * 1    
      } else {
        hpos <- theme$legend.text.align
      }

      grob.labels <- lapply(key$.label, function(label) {
        theme_render(theme, "legend.text", label, hjust = hpos, x = hpos, y = 0.5)
      })

      ## geometory calculation
      if (direction == "vertical") {
        label_width <- do.call("max", lapply(grob.labels, grobWidth))
        label_width <- c(convertWidth(label_width, "mm"))
        label_heights <- do.call("unit.c", lapply(grob.labels, grobHeight))
        label_heights <- c(convertHeight(label_heights, "mm"))

        width <- max(unlist(llply(legend_data, "[[", "size")), 0)
        key_width <- max(key.width, width)

        widths <- c(hgap, key_width, hgap, label_width, max(title_width - key_width - label_width, hgap))
        heights <- c(vgap, title_height, vgap, pmax(key.height, label_heights, key_sizes), vgap)

      } else if(direction == "horizontal") {
        label_width <- do.call("unit.c", lapply(grob.labels, grobWidth))
        label_width <- convertWidth(label_width, "mm")
        label_heights <- do.call("max", lapply(grob.labels, grobHeight))
        label_heights <- convertHeight(label_heights, "mm")

        height <- max(unlist(llply(legend_data, "[[", "size")), 0)
        key_heights <- max(key.height, height)
        key_width <- pmax(key.width, key_sizes)
                                        # width of (key gap label gap) x nkeys
        kglg_width <- do.call("c",lapply(1:length(key_width), function(i)c(key_width[i], hgap, label_width[i], hgap)))
        widths <- c(max(hgap, (title_width - (sum(kglg_width) - hgap)) * 0.5),
                    kglg_width,
                    max(hgap, (title_width - (sum(kglg_width) - hgap)) * 0.5))
        heights <- c(vgap, title_height, vgap, max(key.height, height, label_heights), vgap)
      }

      ## horizontally center is pretty when direction is horizontal
      if (direction == "vertical") {
        hjust <- "left"
      } else if (direction == "horizontal") {
        hjust <- "centre"
      }
      
      .grobs <- list()
      .grobs[[length(.grobs)+1]] <- theme_render(theme, "legend.background")
      
      for (i in 1:nrow(key)) {
        if (direction == "vertical") {
          .grobs[[length(.grobs)+1]] <- theme_render(theme, "legend.key", vp = viewport(layout.pos.col = 2, layout.pos.row = i+3))
        } else if (direction == "horizontal") {
          .grobs[[length(.grobs)+1]] <- theme_render(theme, "legend.key", vp = viewport(layout.pos.col = 1+(i*4)-3, layout.pos.row = 4))
        }
        for(j in seq_along(layers)) {
          if (!is.null(legend_data[[j]])) {
            legend_geom <- Geom$find(layers[[j]]$geom$guide_geom())
            .key <- legend_geom$draw_legend(legend_data[[j]][i, ],
                                           c(layers[[j]]$geom_params, layers[[j]]$stat_params))
            if (direction == "vertical") {
              .key$vp <- viewport(layout.pos.col = 2, layout.pos.row = i+3)
            } else if (direction == "horizontal") {
              .key$vp <- viewport(layout.pos.col = 1+(i*4)-3, layout.pos.row = 4)
            }
            .grobs[[length(.grobs)+1]] <- .key
          }
        }
        grob.labels[[i]]$vp <- if (direction == "vertical")
          viewport(layout.pos.col = 4, layout.pos.row = i+3)
        else if (direction == "horizontal")
          viewport(layout.pos.col = 1+(i*4)-1, layout.pos.row = 4)
      }

      ## Layout the legend table
      legend.layout <- grid.layout(
                                   length(heights), length(widths), 
                                   widths = unit(widths, "mm"), heights = unit(heights, "mm"), 
                                   just = c(hjust, "centre")
                                   )
      grob.title$vp <- viewport(layout.pos.col = 2:(length(widths)-1), layout.pos.row = 2)
      .grobs[[length(.grobs)+1]] <- grob.title
      .grobs <- c(.grobs, grob.labels)
      
      gt <- gTree(children = do.call("gList", .grobs),
                  vp = viewport(layout=legend.layout)
                  )
      gt
    },

    build_legend_data = function(layer, default_mapping) {
      all <- names(c(layer$mapping, default_mapping, layer$stat$default_aes()))
      geom <- c(layer$geom$required_aes, names(layer$geom$default_aes()))
 
      matched <- intersect(intersect(all, geom), names(key))
      matched <- setdiff(matched, names(layer$geom_params))

      if (length(matched) > 0) {
        ## This layer contributes to the legend
        if (is.na(layer$legend) || layer$legend) {
          ## Default is to include it 
          layer$use_defaults(key[matched])
        } else {
          NULL
        }
      } else {
        ## This layer does not contribute to the legend
        if (is.na(layer$legend) || !layer$legend) {
          ## Default is to exclude it
          NULL
        } else {
          layer$use_defaults(NULL)[rep(1, nrow(key)), ]
        }
      }
    }

  )
)

GuideColorbar <- setRefClass("GuideColorbar",
  contains = c("Guide"),
                             
  fields = list(
    nbin = "integer",
    nbreak = "integer",
    bar = "data.frame",
    nodraw.ul = "logical",
    nodraw.ll = "logical"
  ),
                             
  methods = list(
    
    parseScale = function(..., scale, theme) {

      if (!scale$legend || is.null(scale_limits(scale))) return(NULL)

      output <- scale$aesthetics[1]
      title <<- scale$name %||% theme$labels[[output]]

      if (is.null(scale$breaks)) {
        breaks <- pretty(scale_limits(scale), nbreak)
      } else if (is.function(scale$breaks)) {
        breaks <- scale$breaks(limits)
      } else {
        breaks <- scale$breaks
      }
      breaks <- discard(breaks, scale_limits(scale))

      key <<- data.frame(
        scale_map(scale, breaks), I(scale_labels(scale, breaks)), breaks,
        stringsAsFactors = FALSE)
      names(key) <<- c(output, ".label", ".value")

      ## bar specification (number of divs etc)
      .bar <- discard(pretty(scale_limits(scale), n = nbin), scale_limits(scale))
      bar <<- data.frame(colour=scale_map(scale, .bar), value=.bar, stringsAsFactors = FALSE)
    },
    
    hash = function() digest(list(title, key$.label, bar, name)),
    
    mergeGuide = function(new_guide) {
    },
    
    buildGuide = function(layers, default_mapping, theme) {
      
      ## gap between keys etc
      hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
      vgap <- hgap
  
      # Determine key width and height
      if (is.na(theme$legend.key.width)) {
        theme$legend.key.width <- theme$legend.key.size
      }
      key.width <- convertWidth(theme$legend.key.width, "mm")
      key.width.c <- c(key.width)
      
      if (is.na(theme$legend.key.height))
        theme$legend.key.height <- convertHeight(theme$legend.key.size, "mm")
      key.height <- convertHeight(theme$legend.key.height, "mm")
      key.height.c <- c(key.height)

      ## Determine the direction of the elements of legend.
      if (theme$legend.direction == "horizontal") {
        direction <- "horizontal"
      } else {
        direction <- "vertical"
      }

      if (direction=="vertical") {
        bar_height.c <- key.height.c * 5
        bar_width.c <- key.width.c
        
        grob.bar <- rasterGrob(image = bar$colour, width=bar_width.c, height=bar_height.c, default.units = "mm", gp=gpar(col=NA), interpolate = TRUE)
        
        tic_pos_y.c <- rescale(key$.value, c(0.5, nbin-0.5), range(bar$value)) * bar_height.c / nbin
        label_pos_y <- unit(tic_pos_y.c, "mm")
        if (nodraw.ul) tic_pos_y.c <- tic_pos_y.c[-1]
        if (nodraw.ll) tic_pos_y.c <- tic_pos_y.c[-length(tic_pos_y.c)]


        grob.title <- theme_render(
          theme, "legend.title",
          title, hjust = 0, x = 0, y = 0.5)
        ## Compute heights and widths of legend table
        title_width <- convertWidth(grobWidth(grob.title), "mm")
        title_width.c <- c(title_width)
        title_height <- convertHeight(grobHeight(grob.title), "mm")
        title_height.c <- c(title_height)

        grob.label <- theme_render(
          theme, "legend.text", 
          key$.label, x = 0.5, y = label_pos_y)
        label_width <- convertWidth(grobWidth(grob.label), "mm")
        label_width.c <- c(label_width)
        label_height <- convertHeight(grobHeight(grob.label), "mm")
        label_height.c <- c(label_height)

        legend.layout <- grid.layout(
          5, 5,
          widths = c(hgap, bar_width.c, hgap, label_width.c, hgap),
          heights = c(vgap, title_height.c, vgap, bar_height.c, vgap),
                                     just = "left",
          default.unit = "mm")                                     
        
        grob.title$vp <- viewport(layout.pos.col = 2:4, layout.pos.row = 2)
        grob.bar$vp <- viewport(layout.pos.col = 2, layout.pos.row = 4)
        grob.label$vp <- viewport(layout.pos.col = 4, layout.pos.row = 4)
        
        grob.segments <- segmentsGrob(
           x0 = c(rep(0, nbin), rep(bar_width.c * (4/5), nbin)),
           y0 = rep(tic_pos_y.c, 2),
           x1 = c(rep(bar_width.c * (1/5), nbin), rep(bar_width.c, nbin)),
           y1 = rep(tic_pos_y.c, 2),
           default.units = "mm",                                      
           gp = gpar(col="white", lwd=0.5, lineend="butt"),
           vp = viewport(layout.pos.col = 2, layout.pos.row = 4)
                                      )
      } else if (direction=="horizontal") {
        
        bar_height.c <- key.height.c
        bar_width.c <- key.width.c * 5
        
        grob.bar <- rasterGrob(image = t(bar$colour), width=bar_width.c, height=bar_height.c, default.units = "mm", gp=gpar(col=NA), interpolate = TRUE)
        
        tic_pos_y.c <- rescale(key$.value, c(0.5, nbin-0.5), range(bar$value)) * bar_height.c / nbin
        label_pos_y <- unit(tic_pos_y.c, "mm")
        if (nodraw.ul) tic_pos_y.c <- tic_pos_y.c[-1]
        if (nodraw.ll) tic_pos_y.c <- tic_pos_y.c[-length(tic_pos_y.c)]

        tic_pos_x.c <- rescale(key$.value, c(0.5, nbin-0.5), range(bar$value)) * bar_width.c / nbin
        label_pos_x <- unit(tic_pos_x.c, "mm")
        if (nodraw.ul) tic_pos_x.c <- tic_pos_x.c[-1]
        if (nodraw.ll) tic_pos_x.c <- tic_pos_x.c[-length(tic_pos_x.c)]

        grob.title <- theme_render(
          theme, "legend.title",
          title, hjust = 1, x = 1, y = 0.5)
        ## Compute heights and widths of legend table
        title_width <- convertWidth(grobWidth(grob.title), "mm")
        title_width.c <- c(title_width)
        title_height <- convertHeight(grobHeight(grob.title), "mm")
        title_height.c <- c(title_height)

        grob.label <- theme_render(
          theme, "legend.text", 
          key$.label, x = label_pos_x, y = 0.5)
        label_width <- convertWidth(grobWidth(grob.label), "mm")
        label_width.c <- c(label_width)
        label_height <- convertHeight(grobHeight(grob.label), "mm")
        label_height.c <- c(label_height)

        legend.layout <- grid.layout(
          5, 5,
          widths = c(hgap, title_width.c, hgap, bar_width.c, hgap),
          heights = c(vgap, bar_height.c, vgap, label_height.c, vgap),
          just = "left",
          default.unit = "mm")                                     
        
        grob.title$vp <- viewport(layout.pos.col = 2, layout.pos.row = 2:4)
        grob.bar$vp <- viewport(layout.pos.col = 4, layout.pos.row = 2)
        grob.label$vp <- viewport(layout.pos.col = 4, layout.pos.row = 4)
        
        grob.segments <- segmentsGrob(
           x0 = rep(tic_pos_x.c, 2),
           y0 = c(rep(0, nbin), rep(bar_height.c * (4/5), nbin)),
           x1 = rep(tic_pos_x.c, 2),
           y1 = c(rep(bar_height.c * (1/5), nbin), rep(bar_height.c, nbin)),
           default.units = "mm",                                      
           gp = gpar(col="white", lwd=0.5, lineend="butt"),
           vp = viewport(layout.pos.col = 4, layout.pos.row = 2)
                                      )
      }
      
      gTree(
        children = gList(grob.title, grob.bar, grob.label, grob.segments),
        vp = viewport(layout=legend.layout)
      )
    }
  )
)

guide_legend <- function(...) GuideLegend$new(...)
guide_colorbar <- function(nbin = 20, nbreak = 5, nodraw.ul = FALSE, nodraw.ll = FALSE, ...)
  GuideColorbar$new(nbin = nbin, nbreak = nbreak, nodraw.ul = nodraw.ul, nodraw.ll = nodraw.ll, ...)
