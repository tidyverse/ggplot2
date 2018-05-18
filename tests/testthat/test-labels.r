context("Labels")

test_that("setting guide labels works", {

    expect_identical(xlab("my label")$x, "my label")
    expect_identical(labs(x = "my label")$x, "my label")

    expect_identical(ylab("my label")$y, "my label")
    expect_identical(labs(y = "my label")$y, "my label")

    # Plot titles
    expect_identical(labs(title = "my title")$title, "my title")
    expect_identical(labs(title = "my title",
                          subtitle = "my subtitle")$subtitle, "my subtitle")

    # whole plot annotations
    expect_identical(labs(caption = "my notice")$caption, "my notice")
    expect_identical(labs(title = "my title",
                          caption = "my notice")$caption, "my notice")
    expect_identical(labs(tag = "A)")$tag, "A)")
    expect_identical(labs(title = "my title",
                          tag = "A)")$tag, "A)")

    # Colour
    expect_identical(labs(colour = "my label")$colour, "my label")
    # American spelling
    expect_identical(labs(color = "my label")$colour, "my label")
})


# Visual tests ------------------------------------------------------------

test_that("tags are drawn correctly", {
  dat <- data.frame(x = 1:10, y = 10:1)
  p <- ggplot(dat, aes(x = x, y = y)) + geom_point() + labs(tag = "Fig. A)")

  expect_doppelganger("defaults", p)
  expect_doppelganger("Other position", p + theme(plot.tag.position = 'bottom'))
  expect_doppelganger("Manual", p + theme(plot.tag.position = c(0.05, 0.05)))
})
