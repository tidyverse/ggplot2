context("scale_manual")

test_that("names of values used in manual scales", {
   s <- scale_colour_manual(values = c("8" = "c","4" = "a","6" = "b"))
   scale_train(s, c("4", "6", "8"))
   expect_equal(scale_map(s, c("4", "6", "8")), c("a", "b", "c"))  
})