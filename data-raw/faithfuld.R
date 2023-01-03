library(dplyr)

f2d <- MASS::kde2d(faithful$eruptions, faithful$waiting, h = c(1, 10), n = 75)

faithfuld <- expand.grid(eruptions = f2d$x, waiting = f2d$y) %>%
  as_tibble() %>%
  mutate(density = as.vector(f2d$z))

devtools::use_data(faithfuld, overwrite = TRUE)
