test_that("various misuses of +.gg (#2638)", {
  expect_snapshot(error = TRUE, {
    ggplot(mtcars, aes(hwy, displ))
      + geom_point()

    geom_point() + geom_point()
  })
})
