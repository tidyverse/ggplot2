# margins() warn against wrong input lengths

    Code
      margin(c(1, 2), 3, 4, c(5, 6, 7))
    Condition
      Warning:
      In `margin()`, the arguments `t` and `l` should have length 1, not length 2 and 3.
      i Arguments get(s) truncated to length 1.
    Output
      [1] 1points 3points 4points 5points

