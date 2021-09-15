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

    # No extra elements exists
    expect_equivalent(labs(title = "my title"), list(title = "my title"))     # formal argument
    expect_equivalent(labs(colour = "my label"), list(colour = "my label"))   # dot
    expect_equivalent(labs(foo = "bar"), list(foo = "bar"))                   # non-existent param

    # labs() has list-splicing semantics
    params <- list(title = "my title", tag = "A)")
    expect_identical(labs(!!!params)$tag, "A)")

    # NULL is preserved
    expect_equivalent(labs(title = NULL), list(title = NULL))

    # ggtitle works in the same way as labs()
    expect_identical(ggtitle("my title")$title, "my title")
    expect_identical(
      ggtitle("my title", subtitle = "my subtitle")$subtitle,
      "my subtitle"
    )
    expect_equivalent(
      ggtitle("my title", subtitle = NULL),
      list(title = "my title", subtitle = NULL)
    )
})

test_that("Labels from default stat mapping are overwritten by default labels", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_density2d()

  expect_equal(p$labels$colour[1], "colour")
  expect_true(attr(p$labels$colour, "fallback"))

  p <- p + geom_smooth(aes(color = drv))

  expect_equal(p$labels$colour, "drv")
})

test_that("alt text is returned", {
  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point()
  expect_equal(get_alt_text(p), "")
  p <- p + labs(alt = "An alt text")
  expect_equal(get_alt_text(p), "An alt text")
})


# Visual tests ------------------------------------------------------------

test_that("tags are drawn correctly", {
  dat <- data_frame(x = 1:10, y = 10:1)
  p <- ggplot(dat, aes(x = x, y = y)) + geom_point() + labs(tag = "Fig. A)")

  expect_doppelganger("defaults", p)
  expect_doppelganger("Other position", p + theme(plot.tag.position = 'bottom'))
  expect_doppelganger("Manual", p + theme(plot.tag.position = c(0.05, 0.05)))
})
