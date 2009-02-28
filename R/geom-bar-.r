GeomBar <- proto(Geom, {
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionStack
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, weight = 1, alpha = 1)
  
  required_aes <- c("x")
 
  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)
    
    transform(df,
      ymin = 0, ymax = y,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  }
 
  draw_groups <- function(., data, scales, coordinates, ...) {
    GeomRect$draw_groups(data, scales, coordinates, ...)
  }
  
  # Documentation -----------------------------------------------
  objname <- "bar"
  desc <- "Bars, rectangles with bases on y-axis"
  guide_geom <- function(.) "polygon"
  
  icon <- function(.) {
    rectGrob(c(0.3, 0.7), c(0.4, 0.8), height=c(0.4, 0.8), width=0.3, vjust=1, gp=gpar(fill="grey20", col=NA))
  }
  details <- "<p>The bar geom is used to produce 1d area plots: bar charts for categorical x, and histograms for continuous y.  stat_bin explains the details of these summaries in more detail.  In particular, you can use the <code>weight</code> aesthetic to create weighted histograms and barcharts where the height of the bar no longer represent a count of observations, but a sum over some other variable.  See the examples for a practical example.</p>\n<p>By default, multiple x's occuring in the same place will be stacked a top one another by position_stack.  If you want them to be dodged from side-to-side, check out position_dodge.  Finally, position_fill shows relative propotions at each x by stacking the bars and then stretch or squashing them all to the same height</p>\n"
  
  advice <- "<p>If you have presummarised data, use <code>stat=\"identity\" to turn off the default summary</p>\n<p>Sometimes, bar charts are used not as a distributional summary, but instead of a dotplot.  Generally, it's preferable to use a dotplot (see geom_point) as it has a better data-ink ratio.  However, if you do want to create this type of plot, you can set y to the value you have calculated, and use stat='identity'.</p>\n<p>A bar chart maps the height of the bar to a variable, and so the base of the bar must always been shown to produce a valid visual comparison.  Naomi Robbins has a nice <a href='http://www.b-eye-network.com/view/index.php?cid=2468&amp;fc=0&amp;frss=1&amp;ua'>article on this topic</a>.  This is the reason it doesn't make sense to use a log-scaled y axis.</p>\n"
  
  seealso <- list(
    "stat_bin" = "for more details of the binning alogirithm", 
    "position_dodge" = "for creating side-by-side barcharts",
    "position_stack" = "for more info on stacking"
  )
  
  examples <- function(.) {
    # Generate data
    c <- ggplot(mtcars, aes(x=factor(cyl)))
    
    c + geom_bar()
    c + geom_bar() + coord_flip()
    c + geom_bar(fill="white", colour="darkgreen")
    
    # Use qplot
    qplot(factor(cyl), data=mtcars, geom="bar")
    qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))

    # Stacked bar charts    
    qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))
    qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))

    ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar()
    ggplot(diamonds, aes(x=color, fill=cut)) + geom_bar() + coord_flip()
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar()

    # Dodged bar charts    
    ggplot(diamonds, aes(x=clarity, fill=cut)) + geom_bar(position="dodge")
    # compare with 
    ggplot(diamonds, aes(x=cut, fill=cut)) + geom_bar() + facet_grid(. ~ clarity)
    
    # But may be better to use a frequency polygons instead:
    ggplot(diamonds, aes(x=clarity, colour=cut)) + 
      geom_freqpoly(aes(group = cut))
    
    # Often we don't want the height of the bar to represent the
    # count of observations, but the sum of some other variable.
    # For example, the following plot shows the number of diamonds
    # of each colour
    qplot(color, data=diamonds, geom="bar")
    # If, however, we want to see the total number of carats in each colour
    # we need to weight by the carat variable
    qplot(color, data=diamonds, geom="bar", weight=carat, ylab="carat")
    
    # A bar chart used to display means
    meanprice <- tapply(diamonds$price, diamonds$cut, mean)
    cut <- factor(levels(diamonds$cut), levels = levels(diamonds$cut))
    qplot(cut, meanprice)
    qplot(cut, meanprice, geom="bar", stat="identity")
    qplot(cut, meanprice, geom="bar", stat="identity", fill = I("grey50"))
  }  

})