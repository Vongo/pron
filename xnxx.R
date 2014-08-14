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

occurrences <- data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("Key","Count"))), stringsAsFactors=F)
io <- 1
pb <- txtProgressBar(1,length(unique(keys)),1,style=3)
for (k in unique(keys)) {
	occurrences[io,] <- c(k,sum(grepl(k,keys))) # Here
	setTxtProgressBar(pb,io)
	io <- io + 1
}
close(pb)
occurrences <- occurrences[order(occurrences$Count),]
top <- occurrences[1:10000,]

uKeys <- unique(keys)

n <- nrow(top)
# amtx <- matrix(0,nrow=n,ncol=n,dimnames=list(keys,keys))
amtx <- matrix(0,nrow=n,ncol=n)
colnames(amtx) <- top[,"Key"]
rownames(amtx) <- top[,"Key"]

pb <- txtProgressBar(1,length(groups),1,style=3)
ng <- 1
for (group in groups) {
	sequ <- 1:length(group)
	setTxtProgressBar(pb,ng)
	for (i in sequ) {
		term <- group[i]
		if (length(term)>0)
			if (!is.na(term))
				if (term %in% top$Key)
					for (o in sequ[which(sequ>i)]) {
						other <- group[o]
						if (length(other)>0)
							if (!is.na(other))
								if (term %in% top$Key)
									amtx[term,other] <- amtx[term,other]+1
					}
	}
	ng <- ng + 1
}
close(pb)

plot.success <- function(x) qplot(x=colnames(sucksess),y=sucksess[x,],stat="identity",geom="histogram",ylab = "Views per video", xlab = "Year", main = "Evolution of popularity over time")

write.table(amtx,"xnxx_cooccurrences.csv",
		append = FALSE, quote = TRUE, sep = ";",
		eol = "\n", na = "NA", dec = ".", row.names = TRUE,
		col.names = TRUE, qmethod = c("escape", "double"))
