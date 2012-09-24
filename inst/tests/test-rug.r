context("Rug")

test_that("Rugwidth needs unit object", {
    p <- ggplot(mtcars, aes(x=mpg,y=hp))
    expect_is(p + geom_rug(rugwidth=grid::unit(0.01, "npc")), "ggplot")
    expect_error(print(p + geom_rug(rugwidth=0.01)))
})
