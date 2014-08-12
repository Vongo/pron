spleat <- function(x) {
	trunk <- strsplit(x,"'")
	trunk <- ifelse(length(trunk)>1, trunk[2:length(trunk)-1], trunk)
	trunk <- unlist(trunk)
	trunk <- trunk[nchar(trunk)>2]
	trunk
}

data <- read.csv("./xhamster.csv", sep = ",", header = T, stringsAsFactors = F)

keys <- unlist(sapply(data$channels,spleat))
names(keys) <- c()



