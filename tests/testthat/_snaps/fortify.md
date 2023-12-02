# fortify.default proves a helpful error with class uneval

    Code
      ggplot(aes(x = x))
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`, not a <uneval> object.
      i Did you accidentally pass `aes()` to the `data` argument?

