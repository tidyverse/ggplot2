skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("titleGrob() and margins() work correctly", {
  # ascenders and descenders
  g1 <- titleGrob("aaaa", 0, 0, 0.5, 0.5) # lower-case letters, no ascenders or descenders
  g2 <- titleGrob("bbbb", 0, 0, 0.5, 0.5) # lower-case letters, no descenders
  g3 <- titleGrob("gggg", 0, 0, 0.5, 0.5) # lower-case letters, no ascenders
  g4 <- titleGrob("AAAA", 0, 0, 0.5, 0.5) # upper-case letters, no descenders

  expect_equal(height_cm(g1), height_cm(g2))
  expect_equal(height_cm(g1), height_cm(g3))
  expect_equal(height_cm(g1), height_cm(g4))

  # margins
  g5 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g6 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g7 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 0, b = 1, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g8 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 0, b = 0, l = 1, unit = "cm"), margin_x = TRUE, margin_y = TRUE)

  expect_equal(height_cm(g5), height_cm(g1) + 1)
  expect_equal(width_cm(g5), width_cm(g1))
  expect_equal(height_cm(g6), height_cm(g1))
  expect_equal(width_cm(g6), width_cm(g1) + 1)
  expect_equal(height_cm(g7), height_cm(g1) + 1)
  expect_equal(width_cm(g7), width_cm(g1))
  expect_equal(height_cm(g8), height_cm(g1))
  expect_equal(width_cm(g8), width_cm(g1) + 1)

  # no margins when set to false
  g9 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), margin_x = FALSE, margin_y = TRUE)
  g10 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), margin_x = TRUE, margin_y = FALSE)
  expect_equal(height_cm(g9), height_cm(g1) + 2)
  # when one of margin_x or margin_y is set to FALSE and the other to TRUE, then the dimension for FALSE turns into
  # length 1null.
  expect_equal(g9$widths, grid::unit(1, "null"))
  expect_equal(g10$heights, grid::unit(1, "null"))
  expect_equal(width_cm(g10), width_cm(g1) + 2)
})

test_that("margins() warn against wrong input lengths", {
  expect_snapshot(margin(c(1, 2), 3, 4, c(5, 6, 7)))
})

test_that("margin_part() mechanics work as expected", {

  t <- theme_gray() +
    theme(plot.margin = margin_part(b = 11))

  test <- calc_element("plot.margin", t)
  expect_equal(as.numeric(test), c(5.5, 5.5, 11, 5.5))

  t <- theme_gray() +
    theme(margins = margin_part(b = 11))

  test <- calc_element("plot.margin", t)
  expect_equal(as.numeric(test), c(5.5, 5.5, 11, 5.5))
})
