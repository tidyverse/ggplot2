#' Annotation: tick marks
#'
#' This annotation adds tick marks to an axis
#'
#' @export
#' @inheritParams annotation_logticks
#' @param scale character, vector of type of scale attributed to each corresponding side, Default: 'identity'
#' @param ticks_per_base integer, number of minor ticks between each pair of major ticks, Default: NULL
#' @details
#' If scale is of length one it will be replicated to the number of sides given, but if the
#' length of scale is larger than one it must match the number of sides given.
#' If ticks_per_base is set to NULL the function infers the number of ticks per base to be
#' the base of the scale - 1, for example log scale is base exp(1) and
#' log10 and identity are base 10. If ticks_per_base is given it follows the same logic as scale.
#' @examples
#'
#' p <- ggplot(msleep, aes(bodywt, brainwt)) + geom_point()
#'
#' # Default behavior
#'
#' # add identity scale minor ticks on y axis
#' p + annotation_ticks(sides = 'l')
#'
#' # add identity scale minor ticks on x,y axis
#' p + annotation_ticks(sides = 'lb')
#'
#' # Control number of minor ticks of each side independently
#'
#' # add identity scale minor ticks on x,y axis
#' p + annotation_ticks(sides = 'lb', ticks_per_base = c(10,5))
#'
#' # log10 scale
#' p1 <- p + scale_x_log10()
#'
#' # add minor ticks on log10 scale
#' p1 + annotation_ticks(sides = 'b', scale = 'log10')
#'
#' # add minor ticks on both scales
#' p1 + annotation_ticks(sides = 'lb', scale = c('identity','log10'))
#'
#' # add minor ticks on both scales, but force x axis to be identity
#' p1 + annotation_ticks(sides = 'lb', scale = 'identity')
#'
#' # log scale
#' p2 <- p + scale_x_continuous(trans = 'log')
#'
#' # add minor ticks on log scale
#' p2 + annotation_ticks(sides = 'b', scale = 'log')
#'
#' # add minor ticks on both scales
#' p2 + annotation_ticks(sides = 'lb', scale = c('identity','log'))
#'
#' # add minor ticks on both scales, but force x axis to be identity
#' p2 + annotation_ticks(sides = 'lb', scale = 'identity')
#'
#' @import grid
annotation_ticks <- function(sides = "b",
                             scale = 'identity',
                             scaled = TRUE,
                             short = unit(0.1,"cm"),
                             mid = unit(0.2, "cm"),
                             long = unit(0.3, "cm"),
                             colour = "black",
                             size = 0.5,
                             linetype = 1,
                             alpha = 1,
                             color = NULL,
                             ticks_per_base = NULL,
                             ...) {

    if ( !is.null(color) )
        colour <- color

    #check for invalid side
    if( grepl("[^btlr]", sides) )
      stop( gsub('[btlr]','',sides), ' is not a valid side: b,t,l,r are valid')

    #split sides to character vector
    sides <- strsplit(sides,'')[[1]]

    if( length(sides) != length(scale) ){

      if(length(scale)==1){

        scale <- rep(scale,length(sides))

      }else{

        stop('Number of scales does not match the number of sides')

      }

    }

    base <- sapply(scale,function(x) switch(x,'identity'=10,'log10'=10,'log'=exp(1)),USE.NAMES = FALSE)

    if( missing(ticks_per_base) ){

      ticks_per_base <- base - 1

    }else{

      if( (length(sides) != length(ticks_per_base)) ){

        if( length(ticks_per_base)==1 ){

          ticks_per_base <- rep(ticks_per_base,length(sides))

        }else{

          stop('Number of ticks_per_base does not match the number of sides')

        }

      }
    }

    delog <- scale %in% 'identity'

    layer(
      data = data.frame(x = NA),
      mapping = NULL,
      stat = StatIdentity,
      geom = GeomTicks,
      position = PositionIdentity,
      show.legend = FALSE,
      inherit.aes = FALSE,
      params = list(
        base = base,
        sides = sides,
        scaled = scaled,
        short = short,
        mid = mid,
        long = long,
        colour = colour,
        size = size,
        linetype = linetype,
        alpha = alpha,
        ticks_per_base = ticks_per_base,
        delog = delog,
        ...)
      )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTicks <- ggproto("GeomTicks", Geom,
    extra_params = "",
    handle_na = function(data,params) {
    data
},

  draw_panel = function(data,
                        panel_scales,
                        coord,
                        base = c(10, 10),
                        sides = c("b","l"),
                        scaled = TRUE,
                        short = unit(0.1, "cm"),
                        mid = unit(0.2, "cm"),
                        long = unit(0.3, "cm"),
                        ticks_per_base = base - 1,
                        delog = c(x = TRUE, y = TRUE)){

    ticks <- list()

    # Convert these units to numbers so that they can be put in data frames
    short <- convertUnit(short, "cm", valueOnly = TRUE)
    mid <- convertUnit(mid, "cm", valueOnly = TRUE)
    long <- convertUnit(long, "cm", valueOnly = TRUE)

    for ( s in 1:length(sides) ) {
        if ( grepl("[b|t]", sides[s]) ) {

            # Get positions of x tick marks
            xticks <- calc_ticks(base = base[s],
                                 minpow = floor(panel_scales$x.range[1]),
                                 maxpow = ceiling(panel_scales$x.range[2]),
                                 majorTicks = panel_scales$x.major_source,
                                 start = 0,
                                 shortend = short,
                                 midend = mid,
                                 longend = long,
                                 ticks_per_base = ticks_per_base[s],
                                 delog = delog[s])

            if ( scaled ) {
                if ( !delog[s] )
                  xticks$value <- log(xticks$value, base[s])
            }

            names(xticks)[names(xticks) == "value"] <- "x"  # Rename to 'x' for coordinates$transform

            xticks <- coord$transform(xticks, panel_scales)

            # Make the grobs
            if ( grepl("b", sides[s]) ) {
                  ticks$x_b <- with(data,
                                    segmentsGrob(x0 = unit(xticks$x, "native"),
                                                 x1 = unit(xticks$x, "native"),
                                                 y0 = unit(xticks$start, "cm"),
                                                 y1 = unit(xticks$end, "cm"),
                                                 gp = gpar(col = alpha(colour,alpha),
                                                           lty = linetype,
                                                           lwd = size * .pt)
                                                 )
                                    )
            }
            if (grepl("t", sides[s])) {
                  ticks$x_t <- with(data,
                                    segmentsGrob(x0 = unit(xticks$x,"native"),
                                                 x1 = unit(xticks$x, "native"),
                                                 y0 = unit(1, "npc") - unit(xticks$start, "cm"),
                                                 y1 = unit(1, "npc") - unit(xticks$end, "cm"),
                                                 gp = gpar(col = alpha(colour, alpha),
                                                           lty = linetype,
                                                           lwd = size * .pt)
                                                 )
                                    )
            }
        }


        if ( grepl("[l|r]", sides[s]) ) {
            yticks <- calc_ticks(base = base[s],
                                 minpow = floor(panel_scales$y.range[1]),
                                 maxpow = ceiling(panel_scales$y.range[2]),
                                 majorTicks = panel_scales$y.major_source,
                                 start = 0,
                                 shortend = short,
                                 midend = mid,
                                 longend = long,
                                 ticks_per_base = ticks_per_base[s],
                                 delog = delog[s])

            if ( scaled ) {
                if ( !delog[s] ){
                    yticks$value <- log(yticks$value, base[s])
                  }
            }

            names(yticks)[names(yticks) == "value"] <- "y"  # Rename to 'y' for coordinates$transform
            yticks <- coord$transform(yticks, panel_scales)

            # Make the grobs
            if ( grepl("l", sides[s]) ) {
                  ticks$y_l <- with(data,
                                    segmentsGrob(y0 = unit(yticks$y, "native"),
                                                 y1 = unit(yticks$y, "native"),
                                                 x0 = unit(yticks$start, "cm"),
                                                 x1 = unit(yticks$end, "cm"),
                                                 gp = gpar(col = alpha(colour, alpha),
                                                           lty = linetype, lwd = size * .pt)
                                                 )
                                    )
            }
            if ( grepl("r", sides[s]) ) {
                  ticks$y_r <- with(data,
                                    segmentsGrob(y0 = unit(yticks$y, "native"),
                                                 y1 = unit(yticks$y, "native"),
                                                 x0 = unit(1, "npc") - unit(yticks$start, "cm"),
                                                 x1 = unit(1, "npc") - unit(yticks$end, "cm"),
                                                 gp = gpar(col = alpha(colour, alpha),
                                                           lty = linetype,
                                                           lwd = size * .pt)
                                                 )
                                    )
            }
        }
    }
    gTree(children = do.call("gList", ticks))
},
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = 1))


# Calculate the position of log tick marks Returns data frame with: - value: the
# position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0) - end: on the
# other axis, end position of the line (for example, .1, .2, or .3)
calc_ticks <- function(base = 10,
                       ticks_per_base = base - 1,
                       minpow = 0,
                       maxpow = minpow + 1,
                       majorTicks = 0,
                       start = 0,
                       shortend = 0.1,
                       midend = 0.2,
                       longend = 0.3,
                       delog = FALSE){

    # Number of blocks of tick marks
    reps <- maxpow - minpow

    # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
    ticknums <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)

    # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
    powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

    ticks <- ticknums * base^powers

    ticks <- c(ticks, base^maxpow)  # Add the last tick mark

    # Set all of the ticks short
    tickend <- rep(shortend, length(ticks))

    # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
    cycleIdx <- ticknums - 1

    # Set the 'major' ticks long
    tickend[cycleIdx == 0] <- longend

    # Where to place the longer tick marks that are between each base For base 10, this
    # will be at each 5
    longtick_after_base <- floor(ticks_per_base/2)
    tickend[cycleIdx == longtick_after_base] <- midend

    if ( delog ) {

        ticksCopy <- ticks

        regScale <- log(ticks, base)

        majorTicks <- sort(
                            unique(
                                    c(minpow,
                                      regScale[which(regScale %in% majorTicks)],
                                      maxpow,
                                      majorTicks)
                                 )
                          )

        expandScale <- c()

        if ( length(majorTicks) > 1 ) {

            for ( i in 1:(length(majorTicks) - 1) ) {
                expandScale <- c(expandScale,
                                seq(majorTicks[i], majorTicks[i + 1], length.out = (ticks_per_base + 1))
                                )
            }

            ticks <- unique(expandScale)

            # Set all of the ticks short
            tickend <- rep(shortend, length(ticks))

            # Set the 'major' ticks long
            tickend[which(ticks %in% majorTicks)] <- longend
        }
    }

    tickdf <- data.frame(value = ticks, start = start, end = tickend)

    tickdf
}
