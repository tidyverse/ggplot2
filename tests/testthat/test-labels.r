context("Labels")

test_that("Setting guide labels", {

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

    # Colour
    expect_identical(labs(colour = "my label")$colour, "my label")
    # American spelling
    expect_identical(labs(color = "my label")$colour, "my label")
})
