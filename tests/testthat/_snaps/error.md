# various misuses of +.gg (#2638)

    Code
      ggplot(mtcars, aes(hwy, displ))
    Condition
      Error in `geom_blank()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 1st layer.
      Caused by error:
      ! object 'hwy' not found
    Code
      +geom_point()
    Condition
      Error:
      ! Cannot use `+` with a single argument.
      i Did you accidentally put `+` on a new line?
    Code
      geom_point() + geom_point()
    Condition
      Error:
      ! Cannot add <ggproto> objects together.
      i Did you forget to add this object to a <ggplot> object?

