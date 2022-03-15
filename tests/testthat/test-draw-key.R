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

test_that("orientation-aware key glyphs work", {
  df <- data.frame(
    middle = 1:2,
    lower = 0:1,
    upper = 2:3,
    min = -1:0,
    max = 3:4,
    group = c("a","b")
  )

  p_horizontal <- ggplot(df, aes(
    x = middle,
    y = group,
    xmiddle = middle,
    xlower = lower,
    xupper = upper,
    xmin = min,
    xmax = max,
    color = group
  ))

  p_vertical <- ggplot(df, aes(
    x = group,
    y = middle,
    middle = middle,
    lower = lower,
    upper = upper,
    ymin = min,
    ymax = max,
    color = group
  ))

  expect_doppelganger("vertical boxplot",
    p_vertical + geom_boxplot(stat = "identity")
  )
  expect_doppelganger("vertical crossbar",
    p_vertical + geom_crossbar()
  )
  expect_doppelganger("vertical pointrange",
    p_vertical + geom_pointrange()
  )
  expect_doppelganger("vertical linerange",
    p_vertical + geom_linerange()
  )

  expect_doppelganger("horizontal boxplot",
    p_horizontal + geom_boxplot(stat = "identity")
  )
  expect_doppelganger("horizontal crossbar",
    p_horizontal + geom_crossbar()
  )
  expect_doppelganger("horizontal pointrange",
    p_horizontal + geom_pointrange()
  )
  expect_doppelganger("horizontal linerange",
    p_horizontal + geom_linerange()
  )
})
