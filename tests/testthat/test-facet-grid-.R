# General -----------------------------------------------------------------

test_that("facet_grid() accepts vars()", {
  grid <- facet_grid(vars(a = foo))
  expect_identical(grid$params$rows, quos(a = foo))

  grid <- facet_grid(vars(a = foo), vars(b = bar))
  expect_identical(grid$params$rows, quos(a = foo))
  expect_identical(grid$params$cols, quos(b = bar))

  grid <- facet_grid(vars(foo), vars(bar))
  expect_identical(grid$params$rows, quos(foo = foo))
  expect_identical(grid$params$cols, quos(bar = bar))

  expect_equal(facet_grid(vars(am, vs))$params, facet_grid(am + vs ~ .)$params)
  expect_equal(facet_grid(vars(am, vs), vars(cyl))$params, facet_grid(am + vs ~ cyl)$params)
  expect_equal(facet_grid(NULL, vars(cyl))$params, facet_grid(. ~ cyl)$params)
  expect_equal(facet_grid(vars(am, vs), TRUE)$params, facet_grid(am + vs ~ ., margins = TRUE)$params)
})

test_that("facet_grid() handles rows/cols correctly", {
  # fails if passed both a formula and a vars()
  expect_snapshot_error(facet_grid(~foo, vars()))

  # can't pass formulas to `cols`
  expect_snapshot_error(facet_grid(NULL, ~foo))

  # can still pass `margins` as second argument
  grid <- facet_grid(~foo, TRUE)
  expect_true(grid$params$margins)
})

test_that("facet_grid() compact the facet spec, and accept empty spec", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point() +
    facet_grid(vars(NULL))
  d_grid <- get_layer_data(p)

  expect_equal(d_grid$PANEL, factor(c(1L, 1L, 1L)))
  expect_equal(d_grid$group, structure(c(-1L, -1L, -1L), n = 1L))
})

test_that("facets with free scales scale independently", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  # RHS of facet_grid()
  l1 <- p + facet_grid(. ~ z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_true(sd(d1$x) < 1e-10)
  expect_length(unique(d1$y), 3)

  # LHS of facet_grid()
  l2 <- p + facet_grid(z ~ ., scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_length(unique(d2$x), 3)
  expect_true(sd(d2$y) < 1e-10)
})

test_that("facet_grid `axis_labels` argument can be overruled", {

  f <- facet_grid(vars(cyl), axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_grid(vars(cyl), axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = FALSE, y = FALSE))

  # Overrule when only drawing at margins
  f <- facet_grid(vars(cyl), axes = "margins", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

})

test_that("facet_grid `axes` can draw inner axes.", {
  df <- data_frame(
    x = 1:4, y = 1:4,
    fx = c("A", "A", "B", "B"),
    fy = c("c", "d", "c", "d")
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  case <- ggplotGrob(p + facet_grid(vars(fy), vars(fx), axes = "all"))
  ctrl <- ggplotGrob(p + facet_grid(vars(fy), vars(fx), axes = "margins"))

  # 4 x-axes if all axes should be drawn
  bottom <- case$grobs[grepl("axis-b", case$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 4)
  # 2 x-axes if drawing at the margins
  bottom <- ctrl$grobs[grepl("axis-b", ctrl$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 2)

  # Ditto for y-axes
  left <- case$grobs[grepl("axis-l", case$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 4)
  left <- ctrl$grobs[grepl("axis-l", ctrl$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 2)
})

# Layout ------------------------------------------------------------------

a <- data_frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 1))
b <- data_frame(a = 3)
c <- data_frame(b = 3)
empty <- data_frame()
a2 <- data_frame(
  a = factor(1:3, levels = 1:4),
  b = factor(1:3, levels = 4:1),
  c = as.character(c(1:2, NA))
)

panel_layout <- function(facet, data) {
  layout <- create_layout(facet = facet, coord = CoordCartesian)
  layout$setup(data)
  layout$layout
}

test_that("facet_grid() single row and single col are equivalent", {
  row <- panel_layout(facet_grid(a~.), list(a))
  col <- panel_layout(facet_grid(.~a), list(a))

  expect_equal(row$ROW, 1:2)
  expect_equal(row$ROW, col$COL)
  expect_equal(row[c("PANEL", "a")], col[c("PANEL", "a")])

  row <- panel_layout(facet_grid(a~.), list(a, b))
  col <- panel_layout(facet_grid(.~a), list(a, b))

  expect_equal(row$ROW, 1:3)
  expect_equal(row$ROW, col$COL)
  expect_equal(row[c("PANEL", "a")], col[c("PANEL", "a")])
})

test_that("facet_grid() includes all combinations", {
  d <- data_frame(a = c(1, 2), b = c(2, 1))
  all <- panel_layout(facet_grid(a~b), list(d))

  expect_equal(nrow(all), 4)
})

test_that("facet_grid() crossed rows/cols create no more combinations than necessary", {
  facet <- facet_grid(a~b)

  one <- panel_layout(facet, list(a))
  expect_equal(nrow(one), 4)

  one_a <- panel_layout(facet, list(a, empty))
  expect_equal(nrow(one_a), 4)

  two <- panel_layout(facet, list(a, b))
  expect_equal(nrow(two), 4 + 2)

  three <- panel_layout(facet, list(a, b, c))
  expect_equal(nrow(three), 9)

  four <- panel_layout(facet, list(b, c))
  expect_equal(nrow(four), 1)
})


test_that("facet_grid() nested rows/cols create no more combinations than necessary", {
  one <- panel_layout(facet_grid(drv+cyl~.), list(mpg))
  expect_equal(one$PANEL, factor(1:9))
  expect_equal(one$ROW, 1:9)
})

test_that("facet_grid(margins) add correct combinations", {
  one <- panel_layout(facet_grid(a~b, margins = TRUE), list(a))
  expect_equal(nrow(one), 4 + 2 + 2 + 1)
})

test_that("facet_grid(as.table) reverses rows", {
  one <- panel_layout(facet_grid(a~., as.table = FALSE), list(a))
  expect_equal(as.character(one$a), c("2", "1"))

  two <- panel_layout(facet_grid(a~., as.table = TRUE), list(a))
  expect_equal(as.character(two$a), c("1", "2"))
})

test_that("facet_grid(drop = FALSE) preserves unused levels", {
  grid_a <- panel_layout(facet_grid(a~., drop = FALSE), list(a2))
  expect_equal(nrow(grid_a), 4)
  expect_equal(as.character(grid_a$a), as.character(1:4))

  grid_b <- panel_layout(facet_grid(b~., drop = FALSE), list(a2))
  expect_equal(nrow(grid_b), 4)
  expect_equal(as.character(grid_b$b), as.character(4:1))

  grid_ab <- panel_layout(facet_grid(a~b, drop = FALSE), list(a2))
  expect_equal(nrow(grid_ab), 16)
  expect_equal(as.character(grid_ab$a), as.character(rep(1:4, each = 4)))
  expect_equal(as.character(grid_ab$b), as.character(rep(4:1, 4)))
})

test_that("missing values get a panel", {
  a3 <- data_frame(
    a = c(1:3, NA),
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  grid_a <- panel_layout(facet_grid(a~.), list(a3))
  grid_b <- panel_layout(facet_grid(b~.), list(a3))
  grid_c <- panel_layout(facet_grid(c~.), list(a3))

  expect_equal(nrow(grid_a), 4)
  expect_equal(nrow(grid_b), 4)
  expect_equal(nrow(grid_c), 4)
})

test_that("facet_grid() throws errors at bad layout specs", {
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_grid(.~gear, scales = "free") +
    coord_fixed()
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    facet_grid(.~gear, space = "free") +
    theme(aspect.ratio = 1)
  expect_snapshot_error(ggplotGrob(p))
})

test_that("facet_grid() can respect coord aspect with free scales/space", {
  df <- expand.grid(x = letters[1:6], y = LETTERS[1:3])
  p <- ggplot(df, aes(x, y)) +
    geom_tile() +
    facet_grid(
      rows = vars(y == "C"),
      cols = vars(x %in% c("e", "f")),
      scales = "free", space = "free"
    ) +
    coord_fixed(3, expand = FALSE)
  gt <- ggplotGrob(p)
  width  <- gt$widths[panel_cols(gt)$l]
  height <- gt$heights[panel_rows(gt)$t]
  expect_equal(as.numeric(width),  c(4, 2))
  expect_equal(as.numeric(height), c(6, 3))
})

# Data mapping ------------------------------------------------------------

df <- expand.grid(a = 1:2, b = 1:2)
df_a <- unique(df["a"])
df_b <- unique(df["b"])
df_c <- unique(data_frame(c = 1))

panel_map_one <- function(facet, data, plot_data = data) {
  layout <- create_layout(facet = facet, coord = CoordCartesian)
  layout$setup(list(data), plot_data)[[1]]
}

test_that("two col cases with no missings adds a single extra column", {
  loc <- panel_map_one(facet_grid(cyl~vs), mtcars)

  expect_equal(nrow(loc), nrow(mtcars))
  expect_equal(ncol(loc), ncol(mtcars) + 1)

  match <- unique(loc[c("cyl", "vs", "PANEL")])
  expect_equal(nrow(match), 5)
})

test_that("margins add extra data", {
  loc <- panel_map_one(facet_grid(a~b, margins = "b"), df)

  expect_equal(nrow(loc), nrow(df) * 2)

  # For variables including computation (#1864)
  loc <- panel_map_one(facet_grid(a ~ I(b + 1), margins = TRUE), df)
  expect_equal(nrow(loc), nrow(df) * 4)
})

test_that("facet_grid(): missing facet columns are duplicated", {
  facet <- facet_grid(a~b)

  loc_a <- panel_map_one(facet, df_a, plot_data = df)
  expect_equal(nrow(loc_a), 4)
  expect_equal(loc_a$PANEL, factor(c(1, 3, 2, 4)))

  loc_b <- panel_map_one(facet, df_b, plot_data = df)
  expect_equal(nrow(loc_b), 4)
  expect_equal(loc_b$PANEL, factor(1:4))

  loc_c <- panel_map_one(facet, df_c, plot_data = df)
  expect_equal(nrow(loc_c), 4)
  expect_equal(loc_c$PANEL, factor(1:4))
})

test_that("facet_grid can facet by a date/POSIXct variable", {
  date_df <- data_frame(date_var = as.Date(c("1971-12-11", "1987-01-13", "2000-01-01")))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))

  date_df <- data_frame(date_var = as.POSIXct(c("1971-12-11", "1987-01-13", "2000-01-01")))

  grid_col <- facet_grid(~date_var)
  loc_grid_col <- panel_map_one(grid_col, date_df)
  expect_equal(loc_grid_col$PANEL, factor(1:3))

  grid_row <- facet_grid(date_var ~ .)
  loc_grid_row <- panel_map_one(grid_row, date_df)
  expect_equal(loc_grid_row$PANEL, factor(1:3))
})

test_that("facet_grid() respects layer layout", {

  df <- expand.grid(x = LETTERS[1:2], y = 1:3)

  p <- ggplot(df, aes(x, y)) +
    geom_point(colour = "red", layout = "fixed") +
    geom_point(colour = "green", layout = "fixed_rows") +
    geom_point(colour = "purple", layout = "fixed_cols") +
    geom_point() +
    geom_point(colour = "blue", layout = 5) +
    facet_grid(x ~ y)
  b <- ggplot_build(p)

  expect_equal(
    table(get_layer_data(b, i = 1L)$PANEL),
    table(rep(1:6, 6))
  )
  expect_equal(
    table(get_layer_data(b, i = 2L)$PANEL),
    table(rep(1:6, 3))
  )
  expect_equal(
    table(get_layer_data(b, i = 3L)$PANEL),
    table(rep(1:6, 2))
  )
  expect_equal(
    table(get_layer_data(b, i = 4L)$PANEL),
    table(1:6)
  )
  expect_equal(
    table(get_layer_data(b, i = 5L)$PANEL),
    table(factor(5, levels = 1:6))
  )
})

test_that("facet_grid() locates missing values correctly", {
  a3 <- data_frame(
    #  a = c(1:3, NA), Not currently supported
    b = factor(c(1:3, NA)),
    c = factor(c(1:3, NA), exclude = NULL)
  )

  facet <- facet_grid(b~.)
  loc_b <- panel_map_one(facet, data_frame(b = NA), plot_data = a3)
  expect_equal(as.character(loc_b$PANEL), "4")

  facet <- facet_grid(c~.)
  loc_c <- panel_map_one(facet, data_frame(c = NA), plot_data = a3)
  expect_equal(as.character(loc_c$PANEL), "4")
})

test_that("facet_grid() order follows default data frame order", {
  get_layout <- function(p)  ggplot_build(p)@layout$layout

  # Data with factor f with levels CBA
  d <- data_frame(x = 1:9, y = 1:9,
                  fx = factor(rep(letters[1:3], each = 3), levels = letters[3:1]),
                  fy = factor(rep(LETTERS[1:3], each = 3), levels = LETTERS[3:1]))

  # Data with factor f with only level B
  d2 <- data_frame(x = 1:9, y = 2:10, fx = factor("a"), fy = factor("B"))


  # Facets should be in order:
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_grid(fy ~ fx) + geom_point())
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])

  # When adding d2, facets should still be in order:
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(d, aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_blank(data = d2) + geom_point())
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])

  # With no default data: should search each layer in order
  # BCA for rows 1:3
  # acb for cols 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_blank(data = d2) + geom_point(data = d))
  expect_equal(as.character(lay$fy), c("B","C","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("a","c","b")[lay$COL])

  # Same as previous, but different layer order.
  # CBA for rows 1:3
  # cba for cols 1:3
  lay <- get_layout(ggplot(mapping = aes(x, y)) + facet_grid(fy ~ fx) +
                      geom_point(data = d) + geom_blank(data = d2))
  expect_equal(as.character(lay$fy), c("C","B","A")[lay$ROW])
  expect_equal(as.character(lay$fx), c("c","b","a")[lay$COL])
})

# Strips ------------------------------------------------------------------

test_that("facet_grid() lays out strips correctly", {

  strip_layout <- function(p) {
    data <- ggplot_build(p)
    plot <- data@plot
    layout <- data@layout
    data <- data@data
    theme <- plot_theme(plot)

    geom_grobs <- Map(function(l, d) l$draw_geom(d, layout), plot@layers, data)

    facet <- layout$render(geom_grobs, data, theme, plot@labels)
    layout <- facet$layout
    strip_layout <- layout[grepl("^strip", layout$name), 1:4]
    as.list(strip_layout)
  }

  p <- ggplot(mtcars, aes(disp, drat)) + geom_point()

  # Default (top + right)
  grid <- p + facet_grid(~cyl)
  grid_expected <- list(
    t = c(3, 3, 3),
    l = c(3, 5, 7),
    b = c(3, 3, 3),
    r = c(3, 5, 7)
  )
  expect_equal(strip_layout(grid), grid_expected)

  # Switch x (bottom + right)
  grid_x <- p + facet_grid(am ~ cyl, switch = "x")
  grid_x_expected <- list(
    t = c(6, 6, 6, 3, 5),
    l = c(3, 5, 7, 8, 8),
    b = c(6, 6, 6, 3, 5),
    r = c(3, 5, 7, 8, 8)
  )
  expect_equal(strip_layout(grid_x), grid_x_expected)

  # Switch y (top + left)
  grid_y <- p + facet_grid(am ~ cyl, switch = "y")
  grid_y_expected <- list(
    t = c(3, 3, 3, 4, 6),
    l = c(4, 6, 8, 3, 3),
    b = c(3, 3, 3, 4, 6),
    r = c(4, 6, 8, 3, 3)
  )
  expect_equal(strip_layout(grid_y), grid_y_expected)

  # Switch both (bottom + left)
  grid_xy <- p + facet_grid(am ~ cyl, switch = "both")
  grid_xy_expected <- list(
    t = c(6, 6, 6, 3, 5),
    l = c(4, 6, 8, 3, 3),
    b = c(6, 6, 6, 3, 5),
    r = c(4, 6, 8, 3, 3)
  )
  expect_equal(strip_layout(grid_xy), grid_xy_expected)
})

test_that("facet_grid() warns about bad switch input", {
  expect_snapshot_error(facet_grid(am ~ cyl, switch = "z"))
})

test_that("padding is only added if axis is present", {
  p <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
    facet_grid(year ~ drv) +
    theme(
      strip.placement = "outside",
      strip.switch.pad.grid = unit(10, "mm")
    )
  pg <- ggplotGrob(p)
  expect_length(pg$heights, 19)
  expect_length(pg$widths, 18)

  pg <- ggplotGrob(
    p + scale_x_continuous(position = "top") +
      scale_y_continuous(position = "right")
  )
  expect_length(pg$heights, 20)
  expect_equal(as.character(pg$heights[9]), "1cm")
  expect_length(pg$widths, 19)
  expect_equal(as.character(pg$widths[13]), "1cm")

  # Also add padding with negative ticks and no text (#5251)
  pg <- ggplotGrob(
    p + scale_x_continuous(labels = NULL, position = "top") +
      theme(axis.ticks.length.x.top = unit(-2, "mm"))
  )
  expect_length(pg$heights, 20)
  expect_equal(as.character(pg$heights[9]), "1cm")

  # Inverse should be true when strips are switched
  p <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
    facet_grid(year ~ drv, switch = "both") +
    theme(
      strip.placement = "outside",
      strip.switch.pad.grid = unit(10, "mm")
    )

  pg <- ggplotGrob(p)
  expect_length(pg$heights, 20)
  expect_equal(as.character(pg$heights[13]), "1cm")
  expect_length(pg$widths, 19)
  expect_equal(as.character(pg$widths[7]), "1cm")

  pg <- ggplotGrob(
    p + scale_x_continuous(position = "top") +
      scale_y_continuous(position = "right")
  )
  expect_length(pg$heights, 19)
  expect_length(pg$widths, 18)
})

test_that("y strip labels are rotated when strips are switched", {
  switched <- ggplot(mtcars, aes(disp, drat)) +
    geom_point() +
    facet_grid(am ~ cyl, switch = "both")

  expect_doppelganger("switched facet strips", switched)
})

