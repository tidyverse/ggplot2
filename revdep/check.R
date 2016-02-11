library("devtools")

res <- revdep_check(threads = 6)
revdep_check_save_summary()
revdep_check_print_problems()

# revdep_email(date = "Feb 26", only_problems = TRUE, draft = FALSE)
