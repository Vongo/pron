spleat <- function(x) {
	trunk <- strsplit(x,"'")
	trunk <- ifelse(length(trunk)>1, trunk[2:length(trunk)-1], trunk)
	# trunk <- trunk[length(trunk)>1]
	trunk <- unlist(trunk)
	trunk <- trunk[nchar(trunk)>2]
	trunk
}

count.of.key <- function(k) {
	cpt <- 0
	lol <- sapply(sub$channels,function(x){if(grepl(k,x))cpt<<-cpt+1})
	cpt
}

count.views.of.key <- function(k) {
	count <- 0
	for (i in 1:nrow(sub)) {
		r <- sub[i,]
		if(grepl(k,r$channels)){
			count <- count + r$nb_views
		}
	}
	count
}

data <- read.csv("./xhamster.csv", sep = ",", header = T, stringsAsFactors = F)

data$upload_date <- as.Date(data$upload_date,format="%Y-%m-%d")
# data$upload_year <- as.POSIXlt(strptime(data$upload_date,format='%m/%d/%Y'))$year

groups <- sapply(data$channels,spleat)
keys <- unlist(groups)
names(keys) <- c()
keys <- unique(keys)

n <- length(keys)
amtx <- matrix(rep(0,n*n),nrow=n,ncol=n,dimnames=list(keys,keys))

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

nbviews <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("Key","2007","2008","2009","2010","2011","2012","2013"))), stringsAsFactors=F)
times <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("Key","2007","2008","2009","2010","2011","2012","2013"))), stringsAsFactors=F)
pb <- txtProgressBar(1,length(keys),1,style=3)
for (k in 1:length(keys)) {
	key <- keys[k]
	counts <- c()
	views <- c()
	for (y in 2007:2013) {
		beg <- as.Date(paste("01/01/",y,sep=""),format="%m/%d/%Y")
		end <- as.Date(paste("01/01/",y+1,sep=""),format="%m/%d/%Y")
		sub <- data[data$upload_date<end & data$upload_date>beg,]

		counts <- c(counts,count.of.key(key))
		views <- c(views,count.views.of.key(key))
	}
	times[k,] <- c(key,counts)
	nbviews[k,] <- c(key,views)
	setTxtProgressBar(pb,k)
}
close(pb)
sucksess <- nbviews / times