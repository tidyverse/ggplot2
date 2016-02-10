library("devtools")

withr::with_libpaths(getOption("devtools.revdep.libpath"), {
  install_github("hadley/gtable")
  install_github("hadley/scales")
})

res <- revdep_check(threads = 6)
revdep_check_save_summary()
revdep_check_print_problems()
