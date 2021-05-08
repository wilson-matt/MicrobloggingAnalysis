library(lme4)
library(MuMIn)
library(mgcv)

#Retweets model
tweets <- read.csv("All_Tweets_mods.csv")
tweets$text <- NULL
tweets$mentions <- NULL
tweets$hashtags <- NULL
tweets$id <- NULL

tweets$http <- as.factor(tweets$http)
tweets$pics <- as.factor(tweets$pics)

tweets <- tweets[complete.cases(tweets),]
nrow(tweets)

#this is the final global model - 
Global_mod  <- gam(retweets ~ s(username,bs="re") + s(type, bs="re")+ 
                     afraid + amused + angry + annoyed + happy + inspired + sad +
                     http + pics + 
                     mention_count + hashtag_count, 
                   family=nb(theta=0.4118866, link="log"), data=tweets, 
                   method="GACV.Cp", na.action = na.pass, drop.unused.levels=FALSE)

All_models<-dredge(Global_mod, extra = list(
  "R^2", "adjR^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
) # all possible combinations of fixed effects, with R-squared (adjusted) and f statistic 

All_models_RTs <- All_models

Sub_All_models<-subset(All_models_RTs, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.retweets<-model.avg(All_models_RTs, subset=delta<2) # model averaging
Avg.retweets$avg.model # parameter estimates and SEs from average model

print(Avg.retweets)
summary(Avg.retweets)

Sub_All_models


####Favorites Model####

#this is the final global model - use GAM because we don't expect a linear relationship with all predictors.
Global_mod  <- gam(favorites ~ s(username,bs="re") + s(type, bs="re")+ 
                     afraid + amused + angry + annoyed + happy + inspired + sad +
                     http + pics + 
                     mention_count + hashtag_count, 
                   family=nb(theta=0.4768328, link="log"), data=tweets, 
                   method="GACV.Cp", na.action = na.pass, drop.unused.levels=FALSE)


###model averaging
All_models<-dredge(Global_mod, extra = list(
  "R^2", "adjR^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
) # all possible combinations of fixed effects, with R-squared (adjusted) and f statistic 

All_models_Favs <- All_models

Sub_All_models<-subset(All_models_Favs, delta<2) # top models (less than delta AICc = 2 from 'best' model)
Avg.favorites<-model.avg(All_models_Favs, subset=delta<2) # model averaging

print(Avg.favorites)
summary(Avg.favorites)


