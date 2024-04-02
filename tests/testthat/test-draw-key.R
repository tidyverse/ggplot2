# Setting of legend key glyphs has to be tested visually

test_that("alternative key glyphs work", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])

  # specify key glyph by name
  expect_doppelganger("time series and polygon key glyphs",
    ggplot(df, aes(x, y)) +
      geom_line(aes(color = "line"), key_glyph = "timeseries") +
      geom_point(aes(fill = z), pch = 21, size = 3, key_glyph = "polygon") +
      guides(fill = guide_legend(order = 1))
   )

  # specify key glyph by function
  expect_doppelganger("rectangle and dotplot key glyphs",
    ggplot(df, aes(x, y)) +
      geom_line(aes(color = "line"), key_glyph = draw_key_rect) +
      geom_point(aes(fill = z), pch = 21, size = 3, stroke = 2, key_glyph = draw_key_dotplot) +
      guides(fill = guide_legend(order = 1))
  )
})

test_that("keys can communicate their size", {

  draw_key_dummy <- function(data, params, size) {
    grob <- circleGrob(r = unit(1, "cm"))
    attr(grob, "width")  <- 2
    attr(grob, "height") <- 2
    grob
  }

  expect_doppelganger(
    "circle glyphs of 2cm size",
    ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
      geom_point(key_glyph = draw_key_dummy)
  )
})

# Orientation-aware key glyphs --------------------------------------------

test_that("horizontal key glyphs work", {
  df <- data.frame(
    middle = 1:2,
    lower = 0:1,
    upper = 2:3,
    min = -1:0,
    max = 3:4,
    group1 = c("a","b"),
    group2 = c("c","d")
  )

  p <- ggplot(df, aes(
    x = middle,
    xmiddle = middle,
    xlower = lower,
    xupper = upper,
    xmin = min,
    xmax = max
  ))

  expect_doppelganger("horizontal boxplot and crossbar",
    p +
      geom_boxplot(aes(y = group1, color = group1), stat = "identity") +
      geom_crossbar(aes(y = group2, fill = group2)) +
      guides(color = guide_legend(order = 1))
  )
  expect_doppelganger("horizontal linerange and pointrange",
    p +
      geom_linerange(aes(y = group1, color = group1)) +
      geom_pointrange(aes(y = group2, shape = group2)) +
      guides(color = guide_legend(order = 1))
  )
})

test_that("keep_draw_key", {

  key  <- data_frame0(.value = c("A", "C"))
  data <- data_frame0(foo = c("A", "B"), bar = c("B", "C"))

  expect_true( keep_key_data(key, data, "foo", show = TRUE))
  expect_false(keep_key_data(key, data, "foo", show = FALSE))
  expect_equal(keep_key_data(key, data, "foo", show = NA), c(TRUE, FALSE))
  expect_equal(keep_key_data(key, data, "bar", show = NA), c(FALSE, TRUE))
  expect_equal(keep_key_data(key, data, c("foo", "bar"), show = NA), c(TRUE, TRUE))

  # Named show
  expect_true(
    keep_key_data(key, data, c("foo", "bar"), show = c(foo = TRUE, bar = FALSE))
  )
  expect_equal(
    keep_key_data(key, data, c("foo", "bar"), show = c(foo = NA, bar = FALSE)),
    c(TRUE, FALSE)
  )
  expect_equal(
    keep_key_data(key, data, c("foo", "bar"), show = c(foo = FALSE, bar = NA)),
    c(FALSE, TRUE)
  )

  # Missing values
  key  <- data_frame0(.value = c("A", "B", NA))
  data <- data_frame0(foo = c("A", "B", "C")) # 'C' should count as NA
  expect_equal(keep_key_data(key, data, "foo", show = NA), c(TRUE, TRUE, TRUE))

  p <- ggplot(data.frame(x = 1:2), aes(x, x)) +
    geom_point(
      aes(colour = "point", alpha = "point"),
      show.legend = c("colour" = NA, alpha = FALSE)
    ) +
    geom_line(
      aes(colour = "line", alpha = "line"),
      show.legend = c("colour" = NA, alpha = TRUE)
    ) +
    suppressWarnings(scale_alpha_discrete())

  expect_doppelganger("appropriate colour key with alpha key as lines", p)

})
