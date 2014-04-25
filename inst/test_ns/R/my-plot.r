#' Create a plot.
#'
#' @export
#' @import ggplot2
#' @examples
#' plot(my_plot())
my_plot <- function() {
  df <- data.frame(x = 1:10, y = sample(10), z = runif(1))

  ggplot(df) + geom_point(aes_string(x = "x", y = "y", colour = "z"))

}
