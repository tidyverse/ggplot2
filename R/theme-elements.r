#' Theme element: blank.
#' This theme element draws nothing, and assigns no space
#' 
#' @export
element_blank <- function() {
  structure(
    list(),
    class = c("element", "element_any")
  )  
}

#' Theme element: rectangle.
#' 
#' Most often used for backgrounds and borders.
#' 
#' @param fill fill colour
#' @param colour border color
#' @param size border size
#' @param linetype border linetype
#' @export
element_rect <- function(fill = NULL, colour = NULL, size = NULL, linetype = NULL) {
  structure(
    list(fill = fill, colour = colour, size = size, linetype = linetype),
    class = c("element", "element_rect")
  )
}

#' Theme element: line.
#'
#' This element draws a line between two (or more) points
#' 
#' @seealso \code{link{theme_segment}}
#' @param colour line color
#' @param size line size
#' @param linetype line type
#' @export
element_line <- function(colour = NULL, size = NULL, linetype = NULL) {
  structure(
    list(colour = colour, size = size, linetype = linetype),
    class = c("element", "element_line")
  )
}

#' Theme element: segments.
#'
#' This element draws segments between a set of points
#' 
#' @seealso \code{link{theme_line}}
#' @param colour line color
#' @param size line size
#' @param linetype line type
#' @export
element_segment <- function(colour = NULL, size = NULL, linetype = NULL) {
  structure(
    list(colour = colour, size = size, linetype = linetype),
    class = c("element", "element_segment")
  )
}


#' Theme element: text.
#' 
#' @param family font family
#' @param face font face ("plain", "italic", "bold")
#' @param colour text colour
#' @param size text size (in pts)
#' @param hjust horizontal justification (in [0, 1])
#' @param vjust vertical justification (in [0, 1])
#' @param angle angle (in [0, 360])
#' @param lineheight line height
#' @export
element_text <- function(family = NULL, face = NULL, colour = NULL,
  size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL) {

  structure(
    list(family = family, face = face, colour = colour, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight),
    class = c("element", "element_text")
  )
}


# Deprecated theme_xxx functions
theme_blank <- function(...) {
  .Deprecated()
  element_blank(...)
}

theme_rect <- function(...) {
  .Deprecated()
  element_rect(...)
}

theme_line <- function(...) {
  .Deprecated()
  element_line(...)
}

theme_segment <- function(...) {
  .Deprecated()
  element_segment(...)
}

theme_text <- function(...) {
  .Deprecated()
  element_text(...)
}
