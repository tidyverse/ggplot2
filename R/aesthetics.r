#' Aesthetics related to colour: colour, fill
#'
#' This page demonstrates the usage of a sub-group
#' of aesthetics; colour and fill.
#'
#' @rdname aesthetic_colour_fill
#' @name aesthetic_colour_fill
#' @aliases colour color fill aesthetic-colour aesthetic-fill
#' @family aesthetics
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("colour")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("fill")}
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
#'
#' # The aesthetic fill also takes different colouring scales
#' # setting fill equal to a factor varible uses a discrete colour scale
#' k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
#' k + geom_bar()
#'
#' # Fill aesthetic can also be used with a continuous variable
#' m <- ggplot(movies, aes(x = rating))
#' m + geom_histogram()
#' m + geom_histogram(aes(fill = ..count..))
#'
#' # Some geoms don't use both aesthetics (i.e. geom_point or geom_line)
#' b <- ggplot(economics, aes(x = date, y = unemploy))
#' b + geom_line()
#' b + geom_line(colour = "green")
#' b + geom_point()
#' b + geom_point(colour = "red")
#' }
NULL

#' Aesthetic: alpha
#'
#' This page demonstrates the usage of the aesthetic alpha.
#'
#' @rdname aesthetic_alpha
#' @name aesthetic_alpha
#' @aliases alpha aesthetic-alpha
#' @family aesthetics
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("alpha")}
#' @examples
#' \donttest{
#'
#' # For large datasets with overplotting the alpha
#' # aesthetic will make the points more transparent
#' df <- data.frame(x = rnorm(5000), y = rnorm(5000))
#' h  <- ggplot(df, aes(x,y))
#' h + geom_point()
#' h + geom_point(alpha = 0.5)
#' h + geom_point(alpha = 1/10)
#'
#' #If a geom uses both fill and colour, alpha will only modify the fill colour
#' c + geom_bar(fill = "dark grey", colour = "black")
#' c + geom_bar(fill = "dark grey", colour = "black", alpha = 1/3)
#'
#' # Alpha can also be used to add shading
#' j <- b + geom_line()
#' j
#' yrng <- range(economics$unemploy)
#' j <- j + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
#' ymin = yrng[1], ymax = yrng[2], data = presidential)
#' j
#' library(scales) # to access the alpha function
#' j + scale_fill_manual(values = alpha(c("blue", "red"), .3))
#' }
NULL

#' Aesthetics related to grouping: group, order
#'
#' @rdname aesthetic_group_order
#' @name aesthetic_group_order
#' @aliases group order aesthetic-group aesthetic-order
#' @family aesthetics
#' @examples
#' \donttest{
#'
#' # By default, the group is set to the interaction of all discrete variables in the
#' # plot. This often partitions the data correctly, but when it does not, or when
#' # no discrete variable is used in the plot, you will need to explicitly define the
#' # grouping structure, by mapping group to a variable that has a different value
#' # for each group.
#'
#' # For most applications you can simply specify the grouping with
#' # various aesthetics (colour, shape, fill, linetype) or with facets.
#'
#' p <- ggplot(mtcars, aes(wt, mpg))
#' # A basic scatter plot
#' p + geom_point(size = 4)
#' # The colour aesthetic
#' p + geom_point(aes(colour = factor(cyl)), size = 4)
#' # Or you can use shape to distinguish the data
#' p + geom_point(aes(shape = factor(cyl)), size = 4)
#'
#' # Using fill
#' a <- ggplot(mtcars, aes(factor(cyl)))
#' a + geom_bar()
#' a + geom_bar(aes(fill = factor(cyl)))
#' a + geom_bar(aes(fill = factor(vs)))
#'
#' # Using linetypes
#' library(reshape2) # for melt
#' library(plyr) # for colwise
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ec_scaled <- data.frame(
#'   date = economics$date,
#'   colwise(rescale01)(economics[, -(1:2)]))
#' ecm <- melt(ec_scaled, id = "date")
#' f <- ggplot(ecm, aes(date, value))
#' f + geom_line(aes(linetype = variable))
#'
#' # Using facets
#' k <- ggplot(diamonds, aes(carat, ..density..)) + geom_histogram(binwidth = 0.2)
#' k + facet_grid(. ~ cut)
#'
#' # There are three common cases where the default is not enough, and we
#' # will consider each one below. In the following examples, we will use a simple
#' # longitudinal dataset, Oxboys, from the nlme package. It records the heights
#' # (height) and centered ages (age) of 26 boys (Subject), measured on nine
#' # occasions (Occasion).
#'
#' # Multiple groups with one aesthetic
#' library(nlme)
#' h <- ggplot(Oxboys, aes(age, height))
#' # A single line tries to connect all the observations
#' h + geom_line()
#' # The group aesthetic maps a different line for each subject
#' h + geom_line(aes(group = Subject))
#'
#' # Different groups on different layers
#' h <- h + geom_line(aes(group = Subject))
#' # Using the group aesthetic with both geom_line() and geom_smooth()
#' # groups the data the same way for both layers
#' h + geom_smooth(aes(group = Subject), method = "lm", se = FALSE)
#' # Changing the group aesthetic for the smoother layer
#' # fits a single line of best fit across all boys
#' h + geom_smooth(aes(group = 1), size = 2, method = "lm", se = FALSE)
#'
#' # Overriding the default grouping
#' # The plot has a discrete scale but you want to draw lines that connect across
#' # groups. This is the strategy used in interaction plots, profile plots, and parallel
#' # coordinate plots, among others. For example, we draw boxplots of height at
#' # each measurement occasion
#' boysbox <- ggplot(Oxboys, aes(Occasion, height))
#' boysbox + geom_boxplot()
#' # There is no need to specify the group aesthetic here; the default grouping
#' # works because occasion is a discrete variable. To overlay individual trajectories
#' # we again need to override the default grouping for that layer with aes(group = Subject)
#' boysbox <- boysbox + geom_boxplot()
#' boysbox + geom_line(aes(group = Subject), colour = "blue")
#'
#' # Use the order aesthetic to change stacking order of bar charts
#' w <- ggplot(diamonds, aes(clarity, fill = cut))
#' w + geom_bar()
#' w + geom_bar(aes(order = desc(cut)))
#'
#' # Can also be used to change plot order of scatter plots
#' d <- ggplot(diamonds, aes(carat, price, colour = cut))
#' d + geom_point()
#' d + geom_point(aes(order = sample(seq_along(carat))))
#' }
NULL

#' Aesthetic: linetype
#'
#' This page demonstrates the usage of the linetype aesthetic.
#'
#' @rdname aesthetic_linetype
#' @name aesthetic_linetype
#' @aliases linetype aesthetic-linetype
#' @family aesthetics
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("linetype")}
#' @examples
#'
#' # Line types should be specified with either an integer, a name, or with a string of
#' # an even number (up to eight) of hexidecimal digits which give the lengths in
#' # consecutive positions in the string.
#' # 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
#'
#' # Data
#' df <- data.frame(x = 1:10 , y = 1:10)
#' f <- ggplot(df, aes(x = x, y = y))
#' f + geom_line(linetype = 2)
#' f + geom_line(linetype = "dotdash")
#
#' # An example with hex strings, the string "33" specifies three units on followed
#' # by three off and "3313" specifies three units on followed by three off followed
#' # by one on and finally three off.
#' f + geom_line(linetype = "3313")
#'
#' # Mapping line type from a variable
#' library(plyr)
#' library(reshape2)
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ec_scaled <- data.frame(
#'   date = economics$date,
#'   colwise(rescale01)(economics[, -(1:2)]))
#' ecm <- melt(ec_scaled, id = "date")
#' qplot(date, value, data = ecm, geom = "line", linetype = variable)
NULL

#' Aesthetic: size
#'
#' This page demonstrates the usage of the size aesthetic.
#'
#' @rdname aesthetic_size
#' @name aesthetic_size
#' @aliases size aesthetic-size
#' @family aesthetics
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("size")}
#' @examples
#'
#' # Size examples
#' # Should be specified with a numerical value (in millimetres),
#' # or from a variable source
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point(size = 4)
#' p + geom_point(aes(size = qsec))
#' p + geom_point(size = 2.5) + geom_hline(yintercept = 25, size = 3.5)
NULL

#' Aesthetic: shape
#'
#' This page demonstrates the usage of the shape aesthetic.
#'
#' @rdname aesthetic_shape
#' @name aesthetic_shape
#' @aliases shape aesthetic-shape
#' @family aesthetics
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("shape")}
#' @examples
#'
#' # Shape examples
#' # Shape takes four types of values: an integer in [0, 25],
#' # a single character-- which uses that character as the plotting symbol,
#' # a . to draw the smallest rectangle that is visible (i.e., about one pixel)
#' # an NA to draw nothing
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point()
#' p + geom_point(shape = 5)
#' p + geom_point(shape = "k", size = 3)
#' p + geom_point(shape = ".")
#' p + geom_point(shape = NA)
#'
#' # Shape can also be mapped from a variable
#' p + geom_point(aes(shape = factor(cyl)))
#'
#' # A look at all 25 symbols
#' df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
#' s <- ggplot(df2, aes(x = x, y = y))
#' s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()
#' # While all symbols have a foreground colour, symbols 19-25 also take a
#' # background colour (fill)
#' s + geom_point(aes(shape = z), size = 4, colour = "Red") +
#'   scale_shape_identity()
#' s + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
#'   scale_shape_identity()
NULL

#' Aesthetics related to position: x, y, xmin, xmax, ymin, ymax, xend, yend
#'
#' This page demonstrates the usage of a sub-group
#' of aesthetics; x, y, xmin, xmax, ymin, ymax, xend, and yend.
#'
#' @rdname aesthetic_position
#' @name aesthetic_position
#' @aliases x y xmin xmax ymin ymax xend yend aesthetic-x aesthetic-y aesthetic-xmin aesthetic-xmax aesthetic-ymin aesthetic-ymax aesthetic-xend aesthetic-yend
#' @section Geoms:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("x")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("y")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("xmin")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("xmax")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("ymin")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("ymax")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("xend")}
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("yend")}
#' @examples
#'
#' # Generate data: means and standard errors of means for prices
#' # for each type of cut
#' dmod <- lm(price ~ cut, data = diamonds)
#' cuts <- data.frame(cut = unique(diamonds$cut), predict(dmod, data.frame(cut =
#' unique(diamonds$cut)), se = TRUE)[c("fit", "se.fit")])
#' se <- ggplot(cuts, aes(x = cut, y = fit, ymin = fit - se.fit,
#' ymax = fit + se.fit, colour = cut))
#' se + geom_pointrange()
#'
#' # Boxplot with precomputed statistics
#' # generate sample data
#' library(plyr)
#' abc <- adply(matrix(rnorm(100), ncol = 5), 2, quantile, c(0, .25, .5, .75, 1))
#' b <- ggplot(abc, aes(x = X1, ymin = "0%", lower = "25%", middle = "50%", upper = "75%", ymax = "100%"))
#' b + geom_boxplot(stat = "identity")
#'
#' # Using annotate
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + annotate("rect", xmin = 2, xmax = 3.5, ymin = 2, ymax = 25, fill = "dark grey", alpha = .5)
#'
#' # Geom_segment examples
#' library(grid)
#' p + geom_segment(aes(x = 2, y = 15, xend = 2, yend = 25), arrow = arrow(length = unit(0.5, "cm")))
#' p + geom_segment(aes(x = 2, y = 15, xend = 3, yend = 15), arrow = arrow(length = unit(0.5, "cm")))
#' p + geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25), arrow = arrow(length = unit(0.5, "cm")))
#'
#' # You can also use geom_segment to recreate plot(type = "h") :
#' counts <- as.data.frame(table(x = rpois(100, 5)))
#' counts$x <- as.numeric(as.character(counts$x))
#' with(counts, plot(x, Freq, type = "h", lwd = 10))
#'
#' qplot(x, Freq, data = counts, geom = "segment", yend = 0, xend = x, size = I(10))
NULL
