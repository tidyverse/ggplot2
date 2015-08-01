context("Labels")

test_that("Setting guide labels", {

    expect_identical(xlab("my label")$x, "my label")
    expect_identical(labs(x = "my label")$x, "my label")

    expect_identical(ylab("my label")$y, "my label")
    expect_identical(labs(y = "my label")$y, "my label")

    # Colour
    expect_identical(labs(colour = "my label")$colour, "my label")
    # American spelling
    expect_identical(labs(color = "my label")$colour, "my label")
})
