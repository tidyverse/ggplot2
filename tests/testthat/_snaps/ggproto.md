# construction checks input

    Members of a <ggproto> object must have names.

---

    Members of a <ggproto> object must have names.

---

    `_inherit` must be a <ggproto> object, not a <data.frame> object.

---

    Members of a <ggproto> object cannot have duplicate names ("foo").

# ggproto objects print well

    Code
      print(Foo)
    Output
      <ggproto object: Class Foo, gg>
          empty: NULL
          env: environment
          method: function
          num: 12
          theme: theme, ggplot2::theme, gg, S7_object

