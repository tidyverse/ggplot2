vcontext("annotation")

ggplot(mtcars, aes(x=mpg, y=disp)) + 
	geom_line() + 
	annotation_logticks(sides="bt")  + 
	coord_flip()

end_vcontext()
