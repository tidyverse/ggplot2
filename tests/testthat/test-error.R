context("error")

test_that("various misuses of +.gg (#2638)", {
  expect_error(
    {
      ggplot(mtcars, aes(hwy, displ))
      + geom_point()
    },
    "Cannot use `+.gg()` with a single argument. Did you accidentally put + on a new line?",
    fixed = TRUE
  )

  expect_error(
    geom_point() + geom_point(),
    "Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?",
    fixed = TRUE
  )
})
