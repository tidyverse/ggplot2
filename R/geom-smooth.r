#' Add a smoothed conditional mean.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "smooth")}
#'
#' @inheritParams geom_point
#' @inheritParams stat_smooth
#' @param ... Additional arguments passed on to \code{\link{stat_smooth}} and
#'   the underlying statistical model.
#' @seealso The default stat for this geom is \code{\link{stat_smooth}} see
#'   that documentation for more options to control the underlying statistical
#'   model.
#' @export
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_smooth()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", se = FALSE)
#'
#' # See ?stat_smooth for more examples of using built in model fitting
#' # --------------------------------------------------------------
#'
#' # If you need more flexibility, this example shows you how to do
#' # it by hand
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point()
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_smooth(method = "lm", se = FALSE)
#'
#' model <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#' grid <- with(mtcars, expand.grid(
#'   wt = seq(min(wt), max(wt), length.out = 20),
#'   cyl = levels(factor(cyl))
#' ))
#' grid$mpg <- stats::predict(model, newdata = grid)
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_line(data = grid, size = 1)
#'
#' # or with standard errors
#' err <- stats::predict(model, newdata=grid, se = TRUE)
#' grid$ucl <- err$fit + 1.96 * err$se.fit
#' grid$lcl <- err$fit - 1.96 * err$se.fit
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_ribbon(aes(ymin = lcl, ymax = ucl, group = cyl), data = grid,
#'     fill = alpha("grey60", 0.4)) +
#'   geom_line(aes(colour = factor(cyl)), data = grid, size = 1)
geom_smooth <- function(mapping = NULL, data = NULL, stat = "smooth",
  method = "auto", formula = y ~ x, se = TRUE, position = "identity", show_guide = NA,
  inherit.aes = TRUE, ...)
{

  Layer$new(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSmooth,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...),
    stat_params = list(method = method, formula = formula, se = se)
  )
}

GeomSmooth <- proto2(
  class = "GeomSmooth",
  inherit = Geom,
  members = list(
    draw = function(self, data, scales, coordinates, ...) {
      ribbon <- transform(data, colour = NA)
      path <- transform(data, alpha = NA)

      has_ribbon <- function(x) !is.null(data$ymax) && !is.null(data$ymin)

      gList(
        if (has_ribbon(data)) GeomRibbon$draw(ribbon, scales, coordinates),
        GeomLine$draw(path, scales, coordinates)
      )
    },

    guide_geom = function(self) "smooth",

    required_aes = c("x", "y"),

    default_aes = aes(colour = "#3366FF", fill = "grey60", size = 1,
      linetype = 1, weight = 1, alpha = 0.4),

    draw_legend = function(self, data, params, ...) {
      data <- aesdefaults(data, self$default_aes, list(...))
      data$fill <- alpha(data$fill, data$alpha)
      data$alpha <- 1

      if (is.null(params$se) || params$se) {
        gTree(children = gList(
          rectGrob(gp = gpar(col = NA, fill = data$fill)),
          GeomPath$draw_legend(data, ...)
        ))
      } else {
        GeomPath$draw_legend(data, ...)
      }
    }
  )
)
