#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' See \url{http://colorbrewer.org} for more information.
#'
#' @paramCopy ... ggplot2::scale_discrete_x
#' @paramCopy type scales::brewer_pal
#' @paramCopy palette scales::brewer_pal
#' @usageFor scale_colour_brewer
#' @usageFor scale_fill_brewer
#' @export scale_colour_brewer scale_fill_brewer
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- qplot(carat, price, data=dsamp, colour=clarity))
#' 
#' # Change scale label
#' d + scale_colour_brewer()
#' d + scale_colour_brewer("clarity")
#' d + scale_colour_brewer(expression(clarity[beta]))
#' 
#' # Select brewer palette to use, see ?brewer.pal for more details
#' d + scale_colour_brewer(type="seq")
#' d + scale_colour_brewer(type="seq", palette=3)
#' 
#' RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)
#' 
#' d + scale_colour_brewer(palette="Blues")
#' d + scale_colour_brewer(palette="Set1")
#' 
#' # scale_fill_brewer works just the same as 
#' # scale_colour_brewer but for fill colours
#' ggplot(diamonds, aes(x=price, fill=cut)) + 
#'   geom_histogram(position="dodge", binwidth=1000) + 
#'   scale_fill_brewer()
scale_colour_brewer <- function(..., type = "seq", palette = 1) {
  discrete_scale("colour", "brewer", brewer_pal(type, palette), ...)
}
scale_fill_brewer <- function(..., type = "seq", palette = 1) {
  discrete_scale("fill", "brewer", brewer_pal(type, palette), ...)
}

icon.brewer <- function() {
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
    gp=gpar(fill=RColorBrewer::brewer.pal(5, "PuOr"), col=NA)
  )
}
