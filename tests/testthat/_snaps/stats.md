# erroneously dropped aesthetics are found and issue a warning

    Code
      b2 <- ggplot_build(p2)
    Condition
      Warning:
      The following aesthetics were dropped during statistical transformation: colour and fill.
      i This can happen when ggplot fails to infer the correct grouping structure in the data.
      i Did you forget to specify a `group` aesthetic or to convert a numerical variable into a factor?

