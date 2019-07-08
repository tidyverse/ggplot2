context("geom-quantile")
library(tidyverse)
library(quantreg)

test_that("data is ordered by x", {
  set.seed(6531)
  x <- rnorm(10)
  df <- tibble(
    x = x,
    y = x^2 + 0.5 * rnorm(10)
  )
  ps <- ggplot(df, aes(x, y))+  geom_quantile() 
  q.t <- c(0.25,0.5,0.75)
  pred.rq <- predict(rq(y ~ x, tau = q.t, data = df),
  	tibble(x = seq(min(x), max(x), length = 100)))
  pred.rq <- cbind(seq(min(x), max(x), length = 100), pred.rq)
    colnames(pred.rq) <- c("x", paste("Q", q.t * 100, sep = "_"))
  pred.rq <- as_tibble(pred.rq)
  ggplot.data <- as_tibble(layer_data(ps))
  pred.rq.test <- pred.rq[, c("x", "Q_25")]
  colnames(pred.rq.test) <- c("x","y")
  expect_equal(ggplot.data %>% 
  		           filter(quantile == 0.25) %>% 
  		           select(x, y), 
  	           pred.rq.test)
  
  pred.rq.test <- pred.rq[,c("x", "Q_50")]
  colnames(pred.rq.test) <- c("x", "y")
  expect_equal(ggplot.data %>% 
  		           filter(quantile == 0.50) %>% 
  		           select(x, y), 
  	           pred.rq.test)
  
  pred.rq.test <- pred.rq[,c("x", "Q_75")]
  colnames(pred.rq.test) <- c("x", "y")
  expect_equal(ggplot.data %>% 
  		           filter(quantile == 0.75) %>% 
  		           select(x, y), 
  	           pred.rq.test)
})
