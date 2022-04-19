# aes() supports `!!!` in named arguments (#2675)

    Code
      (expect_error(aes(y = 1, !!!list(y = 2))))
    Output
      <simpleError in aes(y = 1, y = 2): formal argument "y" matched by multiple actual arguments>

