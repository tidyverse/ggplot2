vcontext("Alpha")

qplot(1, 1, color = I("#cc000044"), size = I(50))
save_vtest("Alpha set in colour")

qplot(1, 1, color = I("#cc0000"), size = I(50), alpha = I(0.27))
save_vtest("Alpha set in alpha")

end_vcontext()