library("devtools")

res <- revdep_check()
revdep_check_save_summary(res)
