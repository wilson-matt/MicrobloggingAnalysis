####Time series regression ####
setwd(path)
tweets <- read.csv("All_Tweets.csv")
alt_tweets <- read.csv("Agg_alt_rtfavs.csv")
off_tweets <- read.csv("Agg_off_rtfavs.csv")

#off_tweets_post <- off_tweets[ which(as.numeric(off_tweets$jdate) > 389),]
#Exclude first day
off_tweets_post <- off_tweets[ which(as.numeric(off_tweets$jdate) > 390),]
alt_tweets <- alt_tweets[ which(as.numeric(alt_tweets$jdate) > 390),]

alt_tweets[,4:9] <- NULL
colnames(alt_tweets) <-  c("jdate", "alt_retweets", "alt_favorites", "alt_rt_rat", "alt_fav_rat") 
off_tweets_post[,c(1,4:9)] <- NULL

depends <- as.matrix(cbind(alt_tweets[,2:5], off_tweets_post))

  
time_regression <- lm(depends~alt_tweets$jdate)


summary(time_regression)





library(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


tweetfreq_rat <- read.csv("Agg_alt_rtfavs.csv")
tweetfreq_rat$Date <- as.Date(tweetfreq_rat$jdate, origin = as.Date("2015-12-31"))
#tweetfreq_rat$Date <- strptime(as.character(tweetfreq_rat$Date),format="%d-%b-%Y") 
#tweetfreq_rat$Date <- as.Date(tweetfreq_rat$Date,"%d-%b-%Y")
?strptime
tweetfreq_rat2 <- read.csv("Agg_off_rtfavs.csv")
tweetfreq_rat2$Date <- as.Date(tweetfreq_rat2$jdate, origin = as.Date("2015-12-31"))
#
Sys.Date() - 1600 #Must change system date to match (jdate bug with ggplot - it will be further from zero than the current number)
#Total Retweets####
rt_plot <- ggplot(tweetfreq_rat, aes(Date, retweets)) + geom_line(colour="grey40",alpha = 0.5, size=1) +
  xlab(NULL) + ylab("Total retweets")+
  geom_line(data=tweetfreq_rat2, aes(Date, retweets), color="grey60", alpha = 0.5, size=1)+
  geom_line(data=tweetfreq_rat, aes(Date, movretweets, color="Alt"),  alpha = 0.7, size=1, linetype = 2)+
  geom_line(data=tweetfreq_rat2, aes(Date, movretweets, color="Official"), alpha = 0.7, size=1)+
  annotate("text", x=Sys.Date() - 1466, y=78000, label= "a", size=8) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  #  scale_x_continuous(expand = c(0, 0)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y", limits=c(Sys.Date() - 1466, NA))+
  #  scale_y_continuous(expand = c(0, 0))
  scale_y_continuous(limits = c(0, 81000))+
  scale_color_manual(values=c("Alt"="black", "Official"="black"))+
  scale_linetype_manual(values = c("Alt"=2, "Official"=1))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=14))  

?annotate
#Retweets/Tweet####
rt_rat_plot <- ggplot(tweetfreq_rat, aes(Date, rt_rat)) + geom_line(colour="grey40", alpha = 0.5, size=1) +
  xlab("Date") + ylab("Retweets per tweet")+
  geom_line(data=tweetfreq_rat2, aes(Date, rt_rat), colour="grey60", alpha = 0.5, size=1)+
  geom_line(data=tweetfreq_rat, aes(Date, mov_rt_rat), colour="black", alpha = 0.7, size=1, linetype = "dashed")+
  geom_line(data=tweetfreq_rat2, aes(Date, mov_rt_rat), colour="black", alpha = 0.7, size=1)+
  annotate("text", x=Sys.Date() - 1466, y=2500, label= "b", size=8) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y", limits=c(Sys.Date() - 1466, NA))+
  # scale_x_continuous(limits = c(300, 460))+
  scale_y_continuous(limits = c(0, 2700), breaks = c(500, 1000, 1500, 2000, 2500))
#  scale_y_continuous(limits = c(0, 2000))


jpeg("Time_Series_greyscale.jpg", width = 5, height = 8, units = "in", pointsize = 12,
     quality = 75, bg = "white", res= 600)
#attach(mtcars)
#par(mfrow=c(1,2))
multiplot(rt_plot, rt_rat_plot, cols=1)

dev.off()









