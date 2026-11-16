# summary method gives a nice summary

    Code
      summary(p)
    Output
      data: manufacturer, model, displ, year, cyl, trans, drv, cty, hwy, fl,
        class [234x11]
      mapping:  x = ~displ, y = ~hwy, colour = ~drv
      scales:   x, xmin, xmax, xend, xintercept, xmin_final, xmax_final, xlower, xmiddle, xupper, x0, colour 
      faceting:  ~year, ~cyl 
      -----------------------------------
      geom_point: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

