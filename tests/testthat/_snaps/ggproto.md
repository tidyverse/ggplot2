# construction checks input

    Code
      ggproto("Test", NULL, function(self, a) a)
    Condition
      Error in `ggproto()`:
      ! All members of a <ggproto> object must be named.

---

    Code
      ggproto("Test", NULL, a <- (function(self, a) a))
    Condition
      Error in `ggproto()`:
      ! All members of a <ggproto> object must be named.

---

    Code
      ggproto("Test", mtcars, a = function(self, a) a)
    Condition
      Error in `ggproto()`:
      ! `_inherit` must be a <ggproto> object, not a <data.frame> object.

