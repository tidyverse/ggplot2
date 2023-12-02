test_that("geom_label() throws meaningful errors", {
  expect_snapshot_error(geom_label(position = "jitter", nudge_x = 0.5))
  expect_snapshot_error(labelGrob(label = 1:3))
})

test_that("geom_label() rotates labels", {
  df <- data_frame0(
    x = 1:5,
    y = 1,
    lab = c("cat", "dog", "banana", "orange", "tea")
  )

  angle_in <- c(0, 45, 90, 135, 180)

  p <- ggplot(df, aes(x, y, label = lab)) +
    geom_label(angle = angle_in)

  vps <- lapply(
    layer_grob(p, 1)[[1]]$children,
    `[[`, "vp"
  )
  angle_out <- unname(vapply(vps, `[[`, numeric(1), "angle"))
  expect_equal(angle_in, angle_out)
})
