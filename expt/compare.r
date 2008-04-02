info_a <- read.csv("../examples/ex-124/info.csv", stringsAsFactors=FALSE, allowEscapes=TRUE)
info_b <- read.csv("../examples/ex-127/info.csv", stringsAsFactors=FALSE)

info_a$both <- factor(info_a$hash %in% info_b$hash)

qplot(class, geom="bar", data=info_a, fill=both)
qplot(paste(class, obj), geom="bar", data=info_a, fill=both) + coord_flip()
subset(info_a, obj == "path")[, c("both", "src")]

qplot(paste(obj, class), data=info_a, geom="bar", fill=factor(both)) + coord_flip()

info <- merge(info_a, info_b, by="hash", all=T)
# info <- merge(info_a, info_b, by=c("src", "obj"), all=T)

table(table(info_a$hash))
tab <- table(info_a$hash) > 1
dups <- subset(info_a, hash %in% names(tab[tab]))
split(dups$src, dups$hash)
split(dups$src, dups$obj)

files <- gsub("\\.png", "", dir("../examples/ex-113/", "\\.png$"))
setdiff(info_a$hash, files)
setdiff(files, info_a$hash)
matches <- subset(info, !is.na(obj.x) & !is.na(obj.y))

