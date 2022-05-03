test_that("rbind_dfs keep classes of columns", {
  df <- data_frame(
    integer = seq_len(10),
    numeric = as.numeric(seq_len(10)),
    character = letters[1:10],
    factor = factor(letters[1:10]),
    ordered = ordered(letters[1:10]),
    date = Sys.Date()
  )
  df2 <- rbind_dfs(list(df[1:5, ], df[6:10, ]))
  expect_equal(df2, df)
})
