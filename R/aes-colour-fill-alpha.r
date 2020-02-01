#' Colour related aesthetics: colour, fill and alpha
#'
#' This page demonstrates the usage of a sub-group
#' of aesthetics: `colour`, `fill` and `alpha`.
#'
#' @details
#' The `colour` aesthetic changes the colour of a line (or outline),
#' `fill` changes the interior colouring of a geom, `alpha`
#' changes its transparency.
#'
#' `colour` and `fill` aesthetics can be set with a color name or
#' an rgb specification. The alpha aesthetic value goes from 0 to 1,
#' where 0 is completely transparent and 1 is completely opaque.
#'
#' @name aes_colour_fill_alpha
#' @aliases colour color fill
#'
#'    Learn more about setting these aesthetics in `vignette("ggplot2-specs")`.
#'
#'
#' @examples
#' \donttest{
#'
#' # Bar chart example
#' c <- ggplot(mtcars, aes(factor(cyl)))
#' # Default plotting
#' c + geom_bar()
#' # To change the interior colouring use fill aesthetic
#' c + geom_bar(fill = "red")
#' # Compare with the colour aesthetic which changes just the bar outline
#' c + geom_bar(colour = "red")
#' # Combining both, you can see the changes more clearly
#' c + geom_bar(fill = "white", colour = "red")
#' # Both colour and fill can take an rgb specification.
#' c + geom_bar(fill = "#00abff")
#' # Use NA for a completely transparent colour.
#' c + geom_bar(fill = "NA", colour = "#00abff")
#'
#' # The aesthetic fill also takes different colouring scales
#' # setting fill equal to a factor variable uses a discrete colour scale
#' k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
#' k + geom_bar()
#'
#' # Fill aesthetic can also be used with a continuous variable
#' m <- ggplot(faithfuld, aes(waiting, eruptions))
#' m + geom_raster()
#' m + geom_raster(aes(fill = density))
#'
#' # Some geoms don't use both aesthetics (i.e. geom_point or geom_line)
#' b <- ggplot(economics, aes(x = date, y = unemploy))
#' b + geom_line()
#' b + geom_line(colour = "green")
#' b + geom_point()
#' b + geom_point(colour = "red")
#'
#' # For large datasets with overplotting the alpha
#' # aesthetic will make the points more transparent.
#' df <- data.frame(x = rnorm(5000), y = rnorm(5000))
#' h  <- ggplot(df, aes(x,y))
#' h + geom_point()
#' h + geom_point(alpha = 0.5)
#' h + geom_point(alpha = 1/10)
#'
#' # Alpha can also be used to add shading
#' j <- b + geom_line()
#' j
#' yrng <- range(economics$unemploy)
#' j <- j + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
#' ymin = yrng[1], ymax = yrng[2], data = presidential)
#' j
#' j + scale_fill_manual(values = alpha(c("blue", "red"), .3))
#' }
NULL
