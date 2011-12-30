#' Colour related aesthetics: colour, fill and alpha
#' 
#' This page demonstrates the usage of a sub-group 
#' of aesthetics: colour, fill and alpha. 
#'
#' @name aes_colour_fill_alpha
#' @aliases colour color fill alpha 
#' @examples
#' 
#' # Bar chart example:
#' c <- ggplot(mtcars, aes(factor(cyl)))
#' # Default plot
#' c + geom_bar()
#' # To change the interior colouring, use fill aesthetic;
#' # in this example, the fill colour is set to red.
#' c + geom_bar(fill = "red")
#' # Compare with the colour aesthetic which changes just the bar outline:
#' c + geom_bar(colour = "red")
#' # Combining both, you can see the changes more clearly
#' c + geom_bar(fill = "white", colour = "red")
#' 
#' # The fill aesthetic can also be mapped to individual colours
#' # associated with different values of a variable. In the
#' # following example, mapping fill to a factor variable 
#' # produces a discrete colour scale:
#' k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
#' k + geom_bar()
#'
#' # The fill aesthetic can also be mapped to a continuous variable:
#' m <- ggplot(movies, aes(x = rating))
#' m + geom_histogram()
#' m + geom_histogram(aes(fill = ..count..))
#'
#' # The ggplot() call below maps date and unemploy to the
#' # positional x and y aesthetics, respectively.
#' b <- ggplot(economics, aes(x = date, y = unemploy))
#' b + geom_line()
#' # Modify the call to set line color to green:
#' b + geom_line(colour = "green")
#' b + geom_point()
#' # Modify the call to set point color to red:
#' b + geom_point(colour = "red")
#' 
#' # For large datasets with overplotting, the alpha
#' # aesthetic make the points more transparent:
#' df <- data.frame(x = rnorm(5000), y = rnorm(5000))
#' h  <- ggplot(df, aes(x,y))
#' h + geom_point()
#' h + geom_point(colour = alpha("black", .5))
#' h + geom_point(colour = alpha("black", 1/10))
#'
#' #If a geom uses both fill and colour, alpha will only modify the fill colour: 
#' c + geom_bar(fill = "dark grey", colour = "black")
#' c + geom_bar(fill = "dark grey", colour = "black", alpha = 1/3)
#' 
#' # Alpha can also be used to add shading:
#' j <- b + geom_line()
#' j
#' yrng <- range(economics$unemploy)
#' j <- j + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party), 
#' ymin = yrng[1], ymax = yrng[2], data = presidential)
#' j
#' j + scale_fill_manual(values = alpha(c("blue", "red"), .3))
NULL
