vcontext("themes")

p <- qplot(1:3, 1:3)


# Tests for adding theme objects together
# Some of these add directly to ggplot object; others add to theme object first

p + theme_bw() + theme(text=element_text(colour='blue'))
save_vtest("theme_bw() plus blue text")

t <- theme_bw() + theme(text=element_text(colour='blue'))
p + t
save_vtest("add saved theme object with theme_bw() plus blue text")



p + theme(text=element_text(colour='blue')) + theme_bw()
save_vtest("blue text plus theme_bw() - result is black text")

t <- theme(text=element_text(colour='blue')) + theme_bw()
p + t
save_vtest("add saved theme object with blue text plus theme_bw()) - result is black text")



p + theme(text=element_text(colour='blue', face='italic'))
save_vtest("add blue and italic in single element object")


p + theme(text=element_text(colour='blue')) +
    theme(text=element_text(face='italic'))
save_vtest("add blue and italic in separate element objects")


p + theme(text=element_text(colour='blue'),
          text=element_text(face='italic'))
save_vtest("add blue and italic in one theme object with two 'text' elements - result is blue only")



# Inheritance tests

p + theme_bw(base_size=24, base_family="Times") + labs(title="Title text here")
save_vtest('add theme_bw(base_size=24, base_family="Times")')


p + theme_bw() +
    theme(axis.title   = element_text(size=rel(2), colour='blue')) +
    theme(axis.title.x = element_text(size=rel(2)))
save_vtest("axis title text is blue, compounded relative sizing")


# Next two tests contrast the + operator with the %+replace% operator
t <- theme_bw() + theme(axis.title.y = element_text(size = rel(2)))
p + t
save_vtest("theme_bw + larger relative size for axis.title.y")

t <- theme_bw() %+replace% theme(axis.title.y = element_text(size = rel(2)))
p + t
save_vtest("theme_bw %+replace% larger relative size for axis.title.y - result is angle=0")


t <- theme_bw() + theme(text = element_blank())
p + t
save_vtest("text is element_blank - result is no text")


end_vcontext()