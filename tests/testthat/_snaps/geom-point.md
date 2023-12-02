# invalid shape names raise an error

    Code
      translate_shape_string("void")
    Condition
      Error in `translate_shape_string()`:
      ! Shape aesthetic contains invalid value: "void".

---

    Code
      translate_shape_string("tri")
    Condition
      Error in `translate_shape_string()`:
      ! Shape names must be given unambiguously.
      i Fix "tri".

