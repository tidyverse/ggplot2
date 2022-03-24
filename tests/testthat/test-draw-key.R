# Setting of legend key glyphs has to be tested visually

test_that("alternative key glyphs work", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])

  # specify key glyph by name
  expect_doppelganger("time series and polygon key glyphs",
    ggplot(df, aes(x, y)) +
      geom_line(aes(color = "line"), key_glyph = "timeseries") +
      geom_point(aes(fill = z), pch = 21, size = 3, key_glyph = "polygon")
   )

  # specify key glyph by function
  expect_doppelganger("rectangle and dotplot key glyphs",
    ggplot(df, aes(x, y)) +
      geom_line(aes(color = "line"), key_glyph = draw_key_rect) +
      geom_point(aes(fill = z), pch = 21, size = 3, stroke = 2, key_glyph = draw_key_dotplot)
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
      geom_crossbar(aes(y = group2, fill = group2))
  )
  expect_doppelganger("horizontal linerange and pointrange",
    p +
      geom_linerange(aes(y = group1, color = group1)) +
      geom_pointrange(aes(y = group2, shape = group2))
  )
})
