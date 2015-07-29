luv_colours <- as.data.frame(convertColor(t(col2rgb(colors())), "sRGB", "Luv"))
luv_colours$col <- colors()

devtools::use_data(luv_colours, overwrite = TRUE)
