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
