# fortify.default proves a helpful error with class uneval

    `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`, not a <uneval> object.
    i Did you accidentally pass `aes()` to the `data` argument?

# fortify.default can handle healthy data-frame-like objects

    Code
      fortify(X)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` must return an <integer> of length 2.

---

    Code
      fortify(array(1:60, 5:3))
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` must return an <integer> of length 2.

---

    Code
      fortify(cbind(X, Y, Z, deparse.level = 0))
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `colnames(data)` must return a <character> of length `ncol(data)`.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `dim.foo()`:
      ! oops!

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` must return an <integer> of length 2.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` must return an <integer> of length 2.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` can't have `NA`s or negative values.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `dim(data)` can't have `NA`s or negative values.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `dimnames(x)[[2L]]`:
      ! subscript out of bounds

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `colnames(data)` must return a <character> of length `ncol(data)`.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_like()`:
      ! `colnames(data)` must return a <character> of length `ncol(data)`.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `as.data.frame.foo()`:
      ! oops!

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_conversion()`:
      ! `as.data.frame(data)` must return a <data.frame>.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_conversion()`:
      ! `as.data.frame(data)` must preserve dimensions.

---

    Code
      fortify(object)
    Condition
      Error in `fortify()`:
      ! `data` must be a <data.frame>, or an object coercible by `fortify()`, or a valid <data.frame>-like object coercible by `as.data.frame()`.
      Caused by error in `check_data_frame_conversion()`:
      ! `as.data.frame(data)` must preserve column names.

