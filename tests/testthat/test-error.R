test_that("various misuses of +.gg (#2638)", {
  expect_snapshot_error(
    {
      ggplot(mtcars, aes(hwy, displ))
      + geom_point()
    }
  )

  expect_snapshot_error(
    geom_point() + geom_point()
  )
})
