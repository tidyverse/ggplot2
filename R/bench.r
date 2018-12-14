#' Benchmark plot creation time.
#' Broken down into construct, build, render and draw times.
#'
#' @param x code to create ggplot2 plot
#' @export
#' @keywords internal
#' @examples
#' benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point())
#' benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_grid(. ~ cyl))
#'
#' # With tidy eval:
#' p <- expr(ggplot(mtcars, aes(mpg, wt)) + geom_point())
#' benchplot(!!p)

benchplot <- function(x) {
  x <- enquo(x)
  construct <- system.time(x <- rlang::eval_tidy(x))
  stopifnot(inherits(x, "ggplot"))

  build <- system.time(data <- ggplot_build(x))
  render <- system.time(grob <- ggplot_gtable(data))
  draw <- system.time(grid.draw(grob))

  times <- rbind(construct, build, render, draw)[, 1:3]
  times <- rbind(times, colSums(times))

  cbind(
    step = c("construct", "build", "render", "draw", "TOTAL"),
    mat_2_df(times)
  )
}
