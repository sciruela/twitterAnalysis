require(twitteR)
library("ggplot2") 
RT.of.me <- searchTwitter("RT @sciruela", n=100)
news <- getTrends(period="daily")[1:50]
firehose <- publicTimeline(n=999)


my.tweets <- userTimeline('sciruela', n=3500)

head(my.tweets$text)

whence.i.tweet <- sapply( my.tweets, function(x) x$statusSource)

whence.i.tweet<-strsplit(whence.i.tweet,"</a>")
whence.i.tweet<-strsplit(as.character(whence.i.tweet),">")
for(i in 1:length(whence.i.tweet)){
	if(whence.i.tweet!="web"){
		if(length(grep("Twitterrific",whence.i.tweet[i],value=TRUE))!=0){
			whence.i.tweet[i]<-"Twitterrific"
		}
		if(length(grep("Pollowers",whence.i.tweet[i],value=TRUE))!=0){
			whence.i.tweet[i]<-"Pollowers"
		}
		if(length(grep("Tweet Button",whence.i.tweet[i],value=TRUE))!=0){
			whence.i.tweet[i]<-"Tweet Button"
		}
		if(length(grep("Movistar TweetToys",whence.i.tweet[i],value=TRUE))!=0){
			whence.i.tweet[i]<-"Movistar TweetToys"
		}
		if(length(grep("TweepsMap",whence.i.tweet[i],value=TRUE))!=0){
			whence.i.tweet[i]<-"TweepsMap"
		}
	}else{
		whence.i.tweet[i]<-whence.i.tweet[i]
	}
}
pdf("/Users/sciruela/Documents/twitterAnalysis/graph1.pdf")
ggplot( data = data.frame(whence.i.tweet),  aes( x=factor(as.character(whence.i.tweet)),  fill=factor(as.character(whence.i.tweet))))+ scale_y_log10()+ geom_bar()+ coord_polar()+ opts(  title="whence @sciruela tweets",   axis.title.x=theme_blank(),   legend.title=theme_blank()   )
dev.off()

my.tweets <- twListToDF( my.tweets )
iso <- my.tweets$text

require(stringr)
iso.len <- str_length(iso)
pdf("/Users/sciruela/Documents/twitterAnalysis/graph2.pdf")
hist( iso.len, col="lightblue", main="Size of Tweets" )
dev.off()

talkback <- subset( my.tweets,  is.na(replyToSN) == FALSE )

tweeps <- talkback$replyToSN

tweep.count <- table(tweeps)
tweep.levels <- cbind( tweep.count,cut( tweep.count, c(0,1,2,5,100) ),rownames(tweep.count))
tweeps <- data.frame(tweep.levels)
names(tweeps) <- c("number", "category", "name")
class(tweeps$number) <- "numeric"

pdf("/Users/sciruela/Documents/twitterAnalysis/graph3.pdf")
ggplot( data = tweeps, aes(x=number) ) +facet_wrap(~ category, scale="free_x") +geom_text( aes(label=name, y=30-order(name), size=sqrt(log(number)),col=number+(as.numeric(category))^2  ),  position="jitter"  )  +opts( legend.title = theme_blank(), legend.text = theme_blank()  )
dev.off()
