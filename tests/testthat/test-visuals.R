context("Visual tests")

test_that("aesthetics are drawn correctly", {
  dat <- data.frame(xvar = letters[1:3], yvar = 7:9)

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity"),
    "stat='identity'"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", width = 0.5),
    "stat='identity', width=0.5"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar, y = yvar)) + geom_bar(stat = "identity", aes(width = 0.5)),
    "stat='identity', aes(width=0.5)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count"),
    "stat='count'"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count", width = 0.5),
    "stat='count', width=0.5"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = xvar)) + geom_bar(stat = "count", aes(width = 0.5)),
    "stat='count', aes(width=0.5)"
  )
})

test_that("alpha is drawn correctly", {
  vdiffr::expect_doppelganger(
    qplot(1, 1, color = I("#cc000044"), size = I(50)),
    "Alpha set in colour"
  )
  vdiffr::expect_doppelganger(
    qplot(1, 1, color = I("#cc0000"), size = I(50), alpha = I(0.27)),
    "Alpha set in alpha"
  )
})

test_that("aspect ratio is honored", {
  p <- ggplot(data.frame(x = 1:8, y = 1:8, f = gl(2,4), expand.grid(f1 = 1:2, f2 = 1:2, rep = 1:2)), aes(x, y)) + geom_point()

  vdiffr::expect_doppelganger(
    p + theme(aspect.ratio = 3),
    "height is 3 times width"
  )
  vdiffr::expect_doppelganger(
    p + facet_wrap(~f) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 wrap facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(.~f) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 column facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f~.) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 row facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f1~f2) + theme(aspect.ratio = 3),
    "height is 3 times width, 2x2 facets"
  )

  vdiffr::expect_doppelganger(
    p + theme(aspect.ratio = 1/3),
    "width is 3 times height"
  )
  vdiffr::expect_doppelganger(
    p + facet_wrap(~f) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 wrap facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(.~f) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 column facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f~.) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 row facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f1~f2) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2x2 facets"
  )
})

test_that("boxplot draws correctly", {
  vdiffr::expect_doppelganger(
    ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) + geom_boxplot(outlier.size = 5),
    "outlier colours"
  )
})

test_that("cartesian coords draws correctly with limits", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  vdiffr::expect_doppelganger(
    p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50)),
    "expand range"
  )
  vdiffr::expect_doppelganger(
    p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40)),
    "contract range"
  )
})

test_that("maps draws correctly", {
  library(maps)

  # World map
  world_map <- map_data("world")
  pworld <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon()


  vdiffr::expect_doppelganger(
    pworld,
    "no projection"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "mercator"),
    "mercator projection"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "ortho") +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45),
    "ortho projection, default orientation (centered on north pole)"
  )
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "ortho", orientation = c(41, -74 ,0)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45),
    "ortho projection, custom orientation (centered on New York)"
  )
  # Need to set limits here so left-most longitude line shows up
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "aitoff") +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)),
    "aitoff projection, default orientation"
  )
  # This drops half of the world, which probably isn't desirable.
  # It might require rethinking about how limits work.
  vdiffr::expect_doppelganger(
    pworld + coord_map(projection = "aitoff", orientation = c(90, 180, 0)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (0:8) * 45, limits = c(0, 360)),
    "aitoff projection, custom orientation (centered on date line)"
  )
  # USA state map
  states_map <- map_data("state")
  pstate <- ggplot(states_map, aes(x = long, y = lat, group = group))
  vdiffr::expect_doppelganger(
    pstate + geom_polygon() + coord_map("mercator"),
    "USA map, mercator projection"
  )
})

test_that("Polar coordinates draws correctly", {
  dat <- data.frame(x = 0:1, y = rep(1:80, each = 2))

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric circles at theta = 1:80"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y - 80, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric circles at theta = 1:80 - 80"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y - 40, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric circles at theta = 1:80 - 40"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y + 100, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric circles at theta = 1:80 + 100"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y * 100, group = factor(y))) + geom_line() + coord_polar(),
    "Concentric circles at theta = 1:80 * 100"
  )

  dat <- data.frame(
    theta = c(0, 2*pi,   2,   6, 6, 1,    1,  0),
    r     = c(0,    0, 0.5, 0.5, 1, 1, 0.75, .5),
    g     = 1:8)

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(theta, r, colour = g)) + geom_path() +
    geom_point(alpha = 0.3, colour = "black") + coord_polar(),
    "Rays, circular arcs, and spiral arcs"
  )

  dat <- data.frame(x = LETTERS[1:6], y = 11:16)
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(),
    "rose plot with has equal spacing"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(as.numeric(x), y)) + geom_point() + coord_polar(),
    "continuous theta has merged low/high values"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(as.numeric(x), y)) + geom_point() + coord_polar() +
    xlim(0, 6) + ylim(0,16),
    "continuous theta with xlim(0, 6) and ylim(0, 16)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(theta = "y"),
    "racetrack plot with expand=F is closed and doesn't have center hole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") + coord_polar(theta = "y") +
    scale_x_discrete(expand = c(0, 0.6)),
    "racetrack plot with expand=T is closed and has center hole"
  )
})

test_that("geom_dotplot draws correctly", {
  set.seed(112)
  dat <- data.frame(x = rnorm(20), g = LETTERS[1:2])

  # Basic dotplot with binning along x axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4),
    "basic dotplot with dot-density binning, binwidth = .4"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, method = "histodot"),
    "histodot binning (equal bin spacing)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackratio = .5, fill = "white"),
    "dots stacked closer: stackratio=.5, fill=white"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, dotsize = 1.4, fill = "white"),
    "larger dots: dotsize=1.5, fill=white"
  )

  # Stacking methods
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up"),
    "stack up"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down"),
    "stack down"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center"),
    "stack center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole"),
    "stack centerwhole"
  )

  # Stacking methods with coord_flip
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up") + coord_flip(),
    "stack up with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down") + coord_flip(),
    "stack down with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center") + coord_flip(),
    "stack center with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole") + coord_flip(),
    "stack centerwhole with coord_flip"
  )

  # Binning along x, with groups
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4),
    "multiple groups, bins not aligned"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4, binpositions = "all"),
    "multiple groups, bins aligned"
  )

  # Binning along y axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "bin along y, stack center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole"),
    "bin along y, stack centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole", method = "histodot"),
    "bin along y, stack centerwhole, histodot"
  )

  # Binning along y, with multiple grouping factors
  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = LETTERS[1:2])

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "centerwhole"),
    "bin y, three x groups, stack centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all"),
    "bin y, three x groups, bins aligned across groups"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all") +
    coord_flip(),
    "bin y, three x groups, bins aligned, coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
    geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin y, dodged"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
    geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center") +
    coord_flip(),
    "bin y, dodged, coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) + scale_y_continuous(breaks = seq(-4 ,4, .4)) +
    geom_dotplot(binwidth = .2, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin y, three x groups, fill and dodge"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y, group = x)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin y, continous x-axis, grouping by x"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin y, continous x-axis, single x group"
  )

  # Stacking groups
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, binpositions = "all", alpha = 0.5),
    "stackgroups with 3 groups, dot-density with aligned bins"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups with 3 groups, histodot"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(1, y, fill = x)) + geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups with 3 groups, bin y, histodot"
  )

  # This one is currently broken but it would be a really rare case, and it
  # probably requires a really ugly hack to fix
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) +
    geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot",
                 alpha = 0.5, stackdir = "centerwhole"),
    "bin y, dodging, stackgroups with 3 groups, histodot (currently broken)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = g)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5) +
    facet_grid(x ~ .),
    "facets, 3 groups, histodot, stackgroups"
  )

  # Missing values
  dat2 <- dat
  dat2$x[c(1, 10)] <- NA

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x)) + geom_dotplot(binwidth = .4),
    "2 NA values, dot-density binning, binwidth = .4"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "2 NA values, bin along y, stack center"
  )
})

test_that("geom_path draws correctly", {
  set.seed(1)

  nCategory <- 5
  nItem <- 6
  df <- data.frame(category = rep(LETTERS[1:nCategory], 1, each = nItem),
                   item = paste("Item#", rep(1:nItem, nCategory, each = 1), sep = ''),
                   value = rep(1:nItem, nCategory, each = 1) + runif(nCategory * nItem) * 0.8)

  df2 <- df[c(1, 2, 7, 8, 13, 14, 3:6, 9:12, 15:nrow(df)), ]

  vdiffr::expect_doppelganger(
    ggplot(df) + geom_path(aes(x = value, y = category, group = item)),
    "lines"
  )
  vdiffr::expect_doppelganger(
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item)),
    "lines with changed data order, should have same appearance"
  )
  vdiffr::expect_doppelganger(
    ggplot(df) + geom_path(aes(x = value, y = category, group = item, colour = item)),
    "lines, colour"
  )
  vdiffr::expect_doppelganger(
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item, colour = item)),
    "lines, colour, with changed data order, should have same appearance"
  )
})

test_that("geom_polygon draws correctly", {
  vdiffr::expect_doppelganger(
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
    stat_density_2d(aes(colour = ..level..), geom = "path") +
    xlim(0.5, 6) + ylim(40, 110),
    "stat_density2d with paths"
  )
  vdiffr::expect_doppelganger(
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
    xlim(0.5, 6) + ylim(40, 110),
    "stat_density2d with filled polygons"
  )
})

test_that("geom_raster draws correctly", {
  set.seed(1)

  # 3 x 2 ----------------------------------------------------------------------
  df <- data.frame(x = rep(c(-1, 1), each = 3), y = rep(-1:1, 2), z = 1:6)

  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red"),
    "3 x 2"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
    xlim(-2, 2) + ylim(-2, 2),
    "3 x 2, set limits"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
    geom_point(colour = "red"),
    "3 x 2, just = (0, 0)"
  )

  # 1 x 3 ----------------------------------------------------------------------
  df <- data.frame(x = -1:1, y = 0, z = 1:3)

  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red"),
    "1 x 3"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
    xlim(-2, 2) + ylim(-2, 2),
    "1 x 3, set limits"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
    geom_point(colour = "red"),
    "1 x 3, just = (0, 0)"
  )

  # 3 x 1 ----------------------------------------------------------------------

  df <- data.frame(x = 0, y = -1:1, z = 1:3)
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red"),
    "3 x 1"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
    xlim(-2, 2) + ylim(-2, 2),
    "3 x 1, set limits"
  )
  vdiffr::expect_doppelganger(
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
    geom_point(colour = "red"),
    "3 x 1, just = (0, 0)"
  )

  # Categorical fill, irregular swatches ---------------------------------------

  df <- expand.grid(x = 1:10, y = 1:10)
  df$col <- (df$x + df$y) %% 2
  df$col[df$x == 5 & df$col == 1] <- NA
  df$col[df$y == 5 & df$col == 0] <- NA
  vdiffr::expect_doppelganger(
    qplot(x, y, data = df, fill = factor(col), geom = "raster"),
    "irregular categorical"
  )
})

test_that("axis guides are drawn correctly", {
  vdiffr::expect_doppelganger(
    qplot(hwy, reorder(model, hwy), data = mpg) +
    facet_grid(manufacturer ~ ., scales = "free", space = "free") +
    theme(strip.text.y = element_text(angle = 0)),
    "align facet labels, facets horizontal"
  )
  vdiffr::expect_doppelganger(
    qplot(reorder(model, hwy), hwy, data = mpg) +
    facet_grid(. ~ manufacturer, scales = "free", space = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
    "align facet labels, facets vertical"
  )
  vdiffr::expect_doppelganger(
    qplot(wt, mpg, data = mtcars) +
    theme(axis.line = element_line(size = 5, lineend = "square")),
    "thick axis lines"
  )
})

test_that("guides are positioned correctly", {
  p1 <- ggplot(mtcars, aes(mpg, disp, colour = cyl)) +
    geom_point() +
    labs(title = "title of plot") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(breaks = mean(mtcars$mpg), labels = "very very long long axis label") +
    scale_y_continuous(breaks = mean(mtcars$disp), labels = "very very long long axis label")

  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "left"),
    "legend on left"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "bottom"),
    "legend on bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "right"),
    "legend on right"
  )
  vdiffr::expect_doppelganger(
    p1 + theme(legend.position = "top"),
    "legend on top"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "left"),
    "facet_grid, legend on left"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "bottom"),
    "facet_grid, legend on bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "right"),
    "facet_grid, legend on right"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_grid(am~vs) + theme(legend.position = "top"),
    "facet_grid, legend on top"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "left"),
    "facet_wrap, legend on left"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "bottom"),
    "facet_wrap, legend on bottom"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "right"),
    "facet_wrap, legend on right"
  )
  vdiffr::expect_doppelganger(
    p1 + facet_wrap(am~vs) + theme(legend.position = "top"),
    "facet_wrap, legend on top"
  )

  # padding
  dat <- data.frame(x = LETTERS[1:3], y = 1)
  p2 <- ggplot(dat, aes(x, y, fill = x, colour = 1:3)) +
    geom_bar(stat = "identity") +
    theme(legend.background = element_rect(colour = "black")) +
    guides(color = "colorbar")

  vdiffr::expect_doppelganger(
    p2,
    "padding in legend box"
  )

  # Placement of legend inside
  vdiffr::expect_doppelganger(
    p2 + theme(legend.position = c(.5, .5)),
    "legend inside plot, centered"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(0,0), legend.position = c(0,0)),
    "legend inside plot, bottom left"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(1,1), legend.position = c(1,1)),
    "legend inside plot, top right"
  )
  vdiffr::expect_doppelganger(
    p2 + theme(legend.justification = c(0,0), legend.position = c(.5,.5)),
    "legend inside plot, bottom left of legend at center"
  )
})

test_that("straight lines are drawn correctly", {
  dat <- data.frame(x = LETTERS[1:5], y = 1:5)

  # geom_abline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_abline(intercept = 2, slope = 0, colour = "red"),
    "geom_abline: intercept=2, slope=0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_abline(intercept = 0, slope = 1, colour = "red"),
    "geom_abline: intercept=0, slope=1 Should have same values as bars"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_abline(intercept = 2, slope = 0, colour = "red") +
    coord_flip(),
    "geom_abline, coord_flip: intercept=2, slope=0"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    coord_flip(),
    "geom_abline, coord_flip: intercept=0, slope=1, should have same values as bars"
  )

  # geom_hline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_hline(yintercept = 2, colour = "red"),
    "geom_hline: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_hline(yintercept = 2, colour = "red") +
    coord_flip(),
    "geom_hline, coord_flip: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_hline(yintercept = 2, colour = "red") +
    coord_polar(),
    "geom_hline, coord_polar: intercept=2, should have a circle at r=2"
  )

  # geom_vline tests
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_vline(xintercept = 2, colour = "red"),
    "geom_vline: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_vline(xintercept = 2, colour = "red") +
    coord_flip(),
    "geom_vline, coord_flip: intercept=2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, y)) + geom_bar(stat = "identity") +
    geom_vline(xintercept = 2, colour = "red") +
    coord_polar(),
    "geom_vline, coord_polar: intercept=2, should have a ray at 2"
  )

  # hline, vline, and abline tests with coord_map
  library(maps)
  library(mapproj)

  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nzmap <- qplot(x, y, data = nz, geom = "path")

  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(),
    "geom_hline: intercept=-45, projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(),
    "geom_vline: intercept=172, projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(),
    "geom_abline: intercept=130, slope=-1 projection=mercator"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) + coord_map(projection = "cylindrical"),
    "geom_hline: intercept=-45, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) + coord_map(projection = "cylindrical"),
    "geom_vline: intercept=172, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) + coord_map(projection = "cylindrical"),
    "geom_abline: intercept=130, slope=-1, projection=cylindrical"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_hline(yintercept = -45) +
    coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_hline: intercept=-45, projection=azequalarea"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_vline(xintercept = 172) +
    coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_vline: intercept=172, projection=azequalara"
  )
  vdiffr::expect_doppelganger(
    nzmap + geom_abline(intercept = 130, slope = -1) +
    coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0)),
    "geom_abline: intercept=130, slope=-1, projection=azequalarea"
  )
})

test_that("minor breaks draws correctly", {
  p <- ggplot(NULL, aes(1:3, 1:3)) + geom_point() +
    scale_x_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    scale_y_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75))

  vdiffr::expect_doppelganger(
    p,
    "manual minor breaks"
  )
  vdiffr::expect_doppelganger(
    p + coord_polar(),
    "manual minor breaks with coord_polar"
  )

  set.seed(342)
  df <- data.frame(
    date = seq(as.Date("2012-2-29"), length.out = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$date), ]
  library(scales)
  p <- qplot(date, price, data = df, geom = "line") +
    scale_x_date(
      labels = date_format("%m/%d"),
      breaks = date_breaks("month"),
      minor_breaks = date_breaks("week")
    )

  vdiffr::expect_doppelganger(
    p,
    "major breaks: months, minor breaks: weeks"
  )
  vdiffr::expect_doppelganger(
    p + coord_polar(),
    "major breaks: months, minor breaks: weeks, with coord_polar"
  )
  vdiffr::expect_doppelganger(
    ggplot(NULL, aes(letters[1:3], 1:3)) + geom_point(),
    "default breaks"
  )
  vdiffr::expect_doppelganger(
    qplot(1:1e4, 1:1e4) + scale_x_continuous(trans = log2_trans()) + scale_y_log10(),
    "scale_x_continuous(trans = log2_trans()) + scale_y_log10"
  )
  vdiffr::expect_doppelganger(
    qplot(1:5, 1:5) + scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2)),
    "scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2))"
  )
})

test_that("scale breaks can be removed", {
  dat <- data.frame(x = 1:3, y = 1:3)

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_x_continuous(breaks = NULL),
    "no x breaks"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_y_continuous(breaks = NULL),
    "no y breaks"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = 1, y = y, alpha = x)) + geom_point() + scale_alpha_continuous(breaks = NULL),
    "no alpha breaks (no legend)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = 1, y = y, size = x)) + geom_point() + scale_size_continuous(breaks = NULL),
    "no size breaks (no legend)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = 1, y = y, fill = x)) + geom_point(shape = 21) + scale_fill_continuous(breaks = NULL),
    "no fill breaks (no legend)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = 1, y = y, colour = x)) + geom_point() + scale_colour_continuous(breaks = NULL),
    "no colour breaks (no legend)"
  )
})

test_that("date scale draws correctly", {
  set.seed(321)
  df <- data.frame(
    dx = seq(as.Date("2012-02-29"), length.out = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$dx), ]

  dt <- qplot(dx, price, data = df, geom = "line")
  vdiffr::expect_doppelganger(
    dt,
    "dates along x, default breaks"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(breaks = date_breaks("2 weeks")),
    "scale_x_date(breaks = date_breaks(\"2 weeks\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(date_breaks = "3 weeks"),
    "scale_x_date(breaks = \"3 weeks\")"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(labels = date_format("%m/%d")),
    "scale_x_date(labels = date_format(\"%m/%d\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(labels = date_format("%W"), "week"),
    "scale_x_date(labels = date_format(\"%W\"), \"week\")"
  )

  dt <- qplot(price, dx, data = df, geom = "line")
  vdiffr::expect_doppelganger(
    dt,
    "dates along y, default breaks"
  )
  vdiffr::expect_doppelganger(
    dt + scale_y_date(breaks = date_breaks("2 weeks")),
    "scale_y_date(breaks = date_breaks(\"2 weeks\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_y_date(date_breaks = "3 weeks"),
    "scale_y_date(breaks = \"3 weeks\")"
  )
})

test_that("summaries are drawn correctly", {
  vdiffr::expect_doppelganger(
    ggplot(mtcars, aes(x = cyl, y = mpg, colour = factor(vs))) +
    geom_point() +
    stat_summary(fun.y = mean, geom = "line", size = 2),
    "summary with color and lines"
  )
  vdiffr::expect_doppelganger(
    ggplot(mtcars, aes(x = cyl, y = mpg)) +
    geom_point() +
    stat_summary(
      fun.data = mean_cl_boot,
      colour = "red",
      geom = "crossbar",
      width = 0.2
    ),
    "summary with crossbars, no grouping"
  )
  vdiffr::expect_doppelganger(
    ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) +
    geom_point() +
    stat_summary(
      fun.data = mean_cl_boot,
      colour = "red",
      geom = "crossbar",
      width = 0.2
    ),
    "summary with crossbars, manual grouping"
  )
})

test_that("themes are drawn in the right style", {
  library(grid)
  p <- qplot(1:3, 1:3)

  # Tests for adding theme objects together
  # Some of these add directly to ggplot object; others add to theme object first
  vdiffr::expect_doppelganger(
    p + theme_bw() + theme(text = element_text(colour = 'blue')),
    "theme_bw() plus blue text"
  )

  t <- theme_bw() + theme(text = element_text(colour = 'blue'))
  vdiffr::expect_doppelganger(
    p + t,
    "add saved theme object with theme_bw() plus blue text"
  )
  vdiffr::expect_doppelganger(
    p + theme(text = element_text(colour = 'blue')) + theme_bw(),
    "blue text plus theme_bw() - result is black text"
  )

  t <- theme(text = element_text(colour = 'blue')) + theme_bw()
  vdiffr::expect_doppelganger(
    p + t,
    "add saved theme object with blue text plus theme_bw()) - result is black text"
  )
  vdiffr::expect_doppelganger(
    p + theme(text = element_text(colour = 'blue', face = 'italic')),
    "add blue and italic in single element object"
  )
  vdiffr::expect_doppelganger(
    p + theme(
    text = element_text(colour = 'blue')) +
    theme(text = element_text(face = 'italic')
    ),
    "add blue and italic in separate element objects"
  )
  vdiffr::expect_doppelganger(
    p + theme(
    text = element_text(colour = 'blue'),
    text = element_text(face = 'italic')
    ),
    "add blue and italic in one theme object with two 'text' elements - result is blue only"
  )

  # Inheritance tests
  vdiffr::expect_doppelganger(
    p + theme_bw(base_size = 24, base_family = "Times") + labs(title = "Title text here"),
    'add theme_bw(base_size=24, base_family="Times")'
  )
  vdiffr::expect_doppelganger(
    p + theme_bw() +
    theme(axis.title   = element_text(size = rel(2), colour = 'blue')) +
    theme(axis.title.x = element_text(size = rel(2))),
    "axis title text is blue, compounded relative sizing"
  )

  # Next two tests contrast the + operator with the %+replace% operator
  t <- theme_bw() + theme(axis.title.y = element_text(size = rel(2)))
  vdiffr::expect_doppelganger(
    p + t,
    "theme_bw + larger relative size for axis.title.y"
  )

  t <- theme_bw() %+replace% theme(axis.title.y = element_text(size = rel(2)))
  vdiffr::expect_doppelganger(
    p + t,
    "theme_bw %+replace% larger relative size for axis.title.y - result is angle=0"
  )

  t <- theme_bw() + theme(text = element_blank())
  vdiffr::expect_doppelganger(
    p + t,
    "text is element_blank - result is no text"
  )

  # Testing specific elements
  vdiffr::expect_doppelganger(
    p + theme(axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "lightblue"),
      panel.border = element_rect(colour = "black", size = 4, fill = NA)),
    "many blank items, and light blue plot background"
  )
})

test_that("geom_violin draws correctly", {
  set.seed(111)
  dat <- data.frame(x = LETTERS[1:3], y = rnorm(90))
  dat <- dat[dat$x != "C" | c(T, F),]  # Keep half the C's

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(),
    "basic"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(scale = "count"),
    "scale area to sample size (C is smaller)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(width = .5),
    "narrower (width=.5)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(trim = FALSE) + geom_point(shape = 21),
    "with tails and points"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(adjust = .3) + geom_point(shape = 21),
    "with smaller bandwidth and points"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin(),
    "dodging"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_polar(),
    "coord_polar"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_flip(),
    "coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin() + coord_flip(),
    "dodging and coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = as.numeric(x), y = y)) + geom_violin(),
    "continuous x axis, multiple groups (center should be at 2.0)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = as.numeric(1), y = y)) + geom_violin(),
    "continuous x axis, single group (center should be at 1.0)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x=x, y=y)) + geom_violin(draw_quantiles=c(0.25,0.5,0.75)),
    "quantiles"
  )

  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = letters[5:6])
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x = x, y = y, fill = g)) + geom_violin(),
    "grouping on x and fill"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x = x, y = y, fill = g)) +
    geom_violin(position = position_dodge(width = .5)),
    "grouping on x and fill, dodge width = 0.5"
  )
})
