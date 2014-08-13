spleat <- function(x) {
	trunk <- strsplit(x,"'")
	trunk <- ifelse(length(trunk)>2, trunk[2:length(trunk)-1], trunk)
	# trunk <- trunk[length(trunk)>1]
	trunk <- unlist(trunk)
	trunk <- trunk[nchar(trunk)>2]
	trunk
}

count.of.key <- function(k) {
	cpt <- 0
	sapply(sub$tags,function(x){if(grepl(k,x))cpt<<-cpt+1})
	cpt
}

count.views.of.key <- function(k) {
	count <- 0
	apply(sub,1,function(x) {if(grepl(k,x["tags"]))count<<-count+as.numeric(x["nb_comments"])})
	count
}

data <- read.csv("./xnxx.csv", sep = ",", header = T, stringsAsFactors = F)

groups <- sapply(data$tags,spleat)
keys <- unlist(groups)
names(keys) <- c()
keys <- unique(keys)

n <- length(keys)
amtx <- matrix(0,nrow=n,ncol=n,dimnames=list(keys,keys))

for (group in groups) {
	sequ <- 1:length(group)
	for (i in sequ) {
		term <- group[i]
		if (length(term)>0)
			if (!is.na(term))
				for (o in sequ[which(sequ!=i)]) {
					other <- group[o]
					if (length(other)>0)
						if (!is.na(other))
							amtx[term,other] <- amtx[term,other]+1
				}
	}
}

plot.success <- function(x) qplot(x=colnames(sucksess),y=sucksess[x,],stat="identity",geom="histogram",ylab = "Views per video", xlab = "Year", main = "Evolution of popularity over time")

write.table(amtx,"xnxx_cooccurrences.csv",
		append = FALSE, quote = TRUE, sep = ";",
		eol = "\n", na = "NA", dec = ".", row.names = TRUE,
		col.names = TRUE, qmethod = c("escape", "double"))
