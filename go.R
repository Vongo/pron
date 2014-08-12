spleat <- function(x) {
	trunk <- strsplit(x,"'")
	trunk <- ifelse(length(trunk)>1, trunk[2:length(trunk)-1], trunk)
	# trunk <- trunk[length(trunk)>1]
	trunk <- unlist(trunk)
	trunk <- trunk[nchar(trunk)>2]
	trunk
}

count.of.key.for.year <- function(k,y) {
	beg <- as.Date(paste("01/01/",y,sep=""),format="%m/%d/%Y")
	end <- as.Date(paste("01/01/",y+1,sep=""),format="%m/%d/%Y")
	sub <- data[data$upload_date<end & data$upload_date>beg,]
	cpt <- 0
	lol <- sapply(sub$channels,function(x){if(grepl(k,x))cpt<<-cpt+1})
	cpt
}

count.views.of.key.for.year <- function(k,y) {
	beg <- as.Date(paste("01/01/",y,sep=""),format="%m/%d/%Y")
	end <- as.Date(paste("01/01/",y+1,sep=""),format="%m/%d/%Y")
	sub <- data[data$upload_date<end & data$upload_date>beg,]
	count = 0
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
# data$upload_year <- as.POSIXlt(strptime(data$upload_date,format='%m/%d/%Y'))$mday

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

times <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("Key","2007","2008","2009","2010","2011","2012","2013"))), stringsAsFactors=F)
for (k in 1:length(keys)) {
	key <- keys[k]
	counts <- c()
	for (year in 2007:2013) {
		counts <- c(counts,count.of.key.for.year(key,year))
	}
	times[k,] <- c(key,counts)
}

nbviews <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("Key","2007","2008","2009","2010","2011","2012","2013"))), stringsAsFactors=F)
for (k in 1:length(keys)) {
	key <- keys[k]
	counts <- c()
	for (year in 2007:2013) {
		counts <- c(counts,count.views.of.key.for.year(key,year))
	}
	nbviews[k,] <- c(key,counts)
}

sucksess <- nbviews / times