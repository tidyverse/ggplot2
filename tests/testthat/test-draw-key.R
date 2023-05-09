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

test_that("keep_key_data give expected output", {

  key <- data_frame0(
    colour = c("red", "green", "blue"),
    label = c("A", "B", "C")
  )

  data <- list(
    list(
      aesthetics = "colour",
      data = data_frame0(
        pal = c("red", "green", "blue", "orange"),
        member = matrix(c(
          TRUE, TRUE, FALSE, FALSE,
          FALSE, TRUE, TRUE, TRUE
        ), 4, 2)
      )
    )
  )

  expect_equal( # show = TRUE, so keep everything
    keep_key_data(key, data, "colour", show = TRUE, index = 1),
    TRUE
  )
  expect_equal( # check for first index
    keep_key_data(key, data, "colour", show = NA, index = 1),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal( # check for second index
    keep_key_data(key, data, "colour", show = NA, index = 2),
    c(FALSE, TRUE, TRUE)
  )
  expect_equal( # colour = TRUE, not NA, so keep everything
    keep_key_data(key, data, "colour", show = c(label = NA, colour = TRUE), index = 1),
    TRUE
  )
  expect_equal( # colour = NA, so check for colour matches
    keep_key_data(key, data, "colour", show = c(label = TRUE, colour = NA), index = 1),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal( # No relevant key data, keep everything
    keep_key_data(key, data, "label", show = c(label = NA), index = 1),
    TRUE
  )
  data[[2]] <- list(
    aesthetics = "label",
    data = data_frame0(
      pal = c("A", "B", "C"),
      member = matrix(c(
        TRUE,  FALSE, TRUE,
        FALSE, FALSE, TRUE
      ), 3, 2)
    )
  )
  expect_equal( # All keys: colour matches first two, label the third key
    keep_key_data(key, data, c("colour", "label"), show = NA, index = 1),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal( # First key matches neither, so give 1st FALSE
    keep_key_data(key, data, c("colour", "label"), show = NA, index = 2),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("key selection works appropriately", {

  data46 <- subset(mtcars, cyl %in% c(4, 6))
  data68 <- subset(mtcars, cyl %in% c(6, 8))

  p <- ggplot(mapping = aes(disp, mpg, colour = factor(cyl))) +
    geom_point(data = data46) +
    geom_path(data  = data68)

  expect_doppelganger("legend key selection", p)

  p <- ggplot(mapping = aes(disp, mpg, colour = factor(cyl))) +
    geom_point(data = data46, show.legend = TRUE) +
    geom_path(data  = data68)

  expect_doppelganger("partial legend key selection", p)
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
