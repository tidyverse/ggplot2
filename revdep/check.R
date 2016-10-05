# Remember to run from caffeinate R
library("devtools")

to_skip <- skip = c(
  "RcmdrPlugin.FuzzyClust",  # hangs forver on checking if can install
  "TeachingDemos" # hangs forever on checking dependencies in R code
)

revdep_check(threads = 6, skip = to_skip)
revdep_check_save_summary()
revdep_check_print_problems()

# revdep_email(date = "Feb 26", only_problems = TRUE, draft = FALSE)
