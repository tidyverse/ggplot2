#' Blank, draws nothing.
#' 
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots.
#'
#' @name geom_blank
#' @export
#' @examples
#' qplot(length, rating, data=movies, geom="blank")
#' # Nothing to see here!
GeomBlank <- proto(Geom, {
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes()

  draw_legend <- function(., data, ...) {
    zeroGrob()
  }
  
})
