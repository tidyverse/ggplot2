# Rug length needs unit object

    Code
      print(p + geom_rug(length = 0.01))
    Condition
      Error in `geom_rug()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `length` must be a <unit> object, not the number 0.01.

