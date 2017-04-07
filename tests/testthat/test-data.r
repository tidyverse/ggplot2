context("Data")

test_that("stringsAsFactors doesn't affect results", {
    old <- getOption("stringsAsFactors")
    on.exit(options(stringsAsFactors = old), add = TRUE)

    dat.character <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = FALSE)
    dat.factor <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = TRUE)

    base <- ggplot(mapping = aes(x, y)) + geom_point()
    xlabels <- function(x) x$layout$panel_params[[1]]$x.labels

    options(stringsAsFactors = TRUE)
    char_true <- ggplot_build(base %+% dat.character)
    factor_true <- ggplot_build(base %+% dat.factor)

    options(stringsAsFactors = FALSE)
    char_false <- ggplot_build(base %+% dat.character)
    factor_false <- ggplot_build(base %+% dat.factor)

    expect_equal(xlabels(char_true), letters[1:5])
    expect_equal(xlabels(char_false), letters[1:5])
    expect_equal(xlabels(factor_true), letters[1:5])
    expect_equal(xlabels(factor_false), letters[1:5])
})
