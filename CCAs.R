#Create ordinations of term matrix####
library(vegan)
library(ggplot2)
library(ellipse)
library(analogue)

merged_sci_terms <- read.csv("~/All_sci_terms_condensed_count.csv")

merged_sci_terms[is.na(merged_sci_terms)] <- 0
type <- as.factor(c("Alt","Alt","Alt","Alt","Alt","Alt","Alt",
                    "Post","Pre","Post","Pre","Post","Pre","Post",
                    "Pre","Post","Pre","Post","Pre","Post"))
agency <- as.factor(c("Badlands NP", "NOAA", "NPS","USDA","USEPA","USFS",
                      "USFWS","Badlands NP","NOAA","NOAA","NPS","NPS",
                      "USDA","USDA","USEPA","USEPA","USFS","USFS","USFWS","USFWS"))
factors <- as.data.frame(cbind(type,agency))



#All Words
by_count <- merged_sci_terms[,-1]
by_count <- t(by_count[,-1])
colnames(by_count) <- merged_sci_terms$word

#unique(merged_sci_terms$category)
# evidence     climate      agriculture               bird         fossilfuel   agency      
# animal       chem         ecosystem    action       weather      mammal       activity    
# invertebrate ecology      occupation   conservation domestic     pollution    astro       
# energy       education    general      geology      fish         person       plant       
# tech         reptile  

#subset words####
evidence <- subset(merged_sci_terms, grepl("evidence", merged_sci_terms$category))
climate <- subset(merged_sci_terms, grepl("climate", merged_sci_terms$category))
weather <- subset(merged_sci_terms, grepl("weather", merged_sci_terms$category))
agriculture <- subset(merged_sci_terms, grepl("agriculture", merged_sci_terms$category))
fossilfuel <- subset(merged_sci_terms, grepl("fossilefuel", merged_sci_terms$category))
energy <- subset(merged_sci_terms, grepl("energy", merged_sci_terms$category))
action <- subset(merged_sci_terms, grepl("action", merged_sci_terms$category))
chem <- subset(merged_sci_terms, grepl("chem", merged_sci_terms$category))
pollution <- subset(merged_sci_terms, grepl("pollution", merged_sci_terms$category))

ecosystem <- subset(merged_sci_terms, grepl("ecosystem", merged_sci_terms$category))
ecology <- subset(merged_sci_terms, grepl("ecology", merged_sci_terms$category))
conservation <- subset(merged_sci_terms, grepl("conservation", merged_sci_terms$category))
geology <- subset(merged_sci_terms, grepl("geology", merged_sci_terms$category))

bird <- subset(merged_sci_terms, grepl("bird", merged_sci_terms$category))
animal <- subset(merged_sci_terms, grepl("animal", merged_sci_terms$category))
mammal <- subset(merged_sci_terms, grepl("mammal", merged_sci_terms$category))
invertebrate <- subset(merged_sci_terms, grepl("invertebrate", merged_sci_terms$category))
domestic <- subset(merged_sci_terms, grepl("domestic", merged_sci_terms$category))
fish <- subset(merged_sci_terms, grepl("fish", merged_sci_terms$category))
plant <- subset(merged_sci_terms, grepl("plant", merged_sci_terms$category))
reptile <- subset(merged_sci_terms, grepl("reptile", merged_sci_terms$category))

astro <- subset(merged_sci_terms, grepl("astro", merged_sci_terms$category))
education <- subset(merged_sci_terms, grepl("education", merged_sci_terms$category))
general <- subset(merged_sci_terms, grepl("general", merged_sci_terms$category))

by_count_orgs <- rbind(bird, animal, mammal, invertebrate, domestic, fish, plant, reptile)
###No agency
test <- subset(merged_sci_terms, !grepl("agency", merged_sci_terms$category))

#
by_count_sub <- test
row.names(by_count_sub) <- by_count_sub$word
by_count_sub <- by_count_sub[,-1]
by_count_sub <- t(by_count_sub[,-1])
by_count_sub <- by_count_sub[rowSums(by_count_sub) >0,]

#####
#for NMDS
#ord <- metaMDS(by_count)
#site_scores <- scores(ord, display="sites") 
#species_scores <- scores(ord, display="species") 
#rownames(species_scores) <- merged_sci_terms$word
#for RDA/CCA
#The traditional view of CA and CCA as "unimodal methods" has been questioned by 
#ter Braak and Smilauer (1998), who demonstrate that CA and CCA  have two "faces": 
#a unimodal face and a linear face.  Therefore, we do not need to be too concerned 
#whether our underlying model is linear vs. unimodal before applying CA and CCA.  
#However, we do need to be aware that the linear face of CA/CCA focusses on species 
#composition (i.e. relative data), rather than overall trends in abundance.  PCA 
#and RDA would be more appropriate for such data.
#SO we used CCA because we are more concerned w/ relative trends rather than absolutes...uneven sample sizes.
#use CCA for uni-modal and/or linear responses and uneven samples
#RDA only for linear response and even samples (so density not rel abund)
#ncol(by_count_sub)

ord <- cca(by_count_sub~agency+type)
anova.cca(ord, by="terms")
RsquareAdj(ord)
varExpl(ord, axes=1L)
varExpl(ord, axes=2L)

#source("https://raw.githubusercontent.com/cran/adespatial/master/R/forward.sel.par.R")
#forward.sel.par(by_count, cbind(agency, type), adjR2thresh=r2a)
#mod.step <- step(ord, scope = by_count~type+agency, direction="both")
?anova.cca
a<-data.frame(Axis1 = scores(ord)$sites[ , 1])
b<-data.frame(Axis2 = scores(ord)$sites[ , 2])
#c<-data.frame(contraining_variable[,1])
site_scores <-cbind(a,b)

a<-data.frame(Axis1 = scores(ord)$species[ , 1])
b<-data.frame(Axis2 = scores(ord)$species[ , 2])
species_scores <-cbind(a,b)

species_list <- merged_sci_terms$word

#Ax1 <- as.matrix(rbind(-0.287216560,-0.971184719,-0.129029062,-0.013637089,
#                       -0.260695757,1.008863043,-0.315779955,-0.610374688,-0.695995896))

#Ax2 <- as.matrix(rbind(0.196239900, 0.785950922,-0.549265681,-0.110445415,
#                       -0.171178580,0.301019774,-0.001355804,-0.975322994,0.267574068))

#LogSpeciesScores <-cbind(Ax1,Ax2)
#make category centroids to plot
categories <- test

species_scores <- scores(ord)$species
word <- rownames(species_scores)
species_scores <- as.data.frame(cbind(word, species_scores))


words_by_scores <- as.data.frame(merge(species_scores, categories, by = "word"))
categories_cca1 <- as.matrix(aggregate(as.numeric(as.character(CCA1)) ~ category, data=words_by_scores, 
                                       FUN = function(x) c(mean=mean(x), SD=sd(x))))
categories_cca2 <- as.matrix(aggregate(as.numeric(as.character(CCA2)) ~ category, data=words_by_scores, 
                                       FUN = function(x) c(mean=mean(x), SD=sd(x))))
category_centroids <- as.data.frame(cbind(categories_cca1,categories_cca2))
colnames(category_centroids) <- c("category","CCA1_mean", "CCA1_sd", "category", "CCA2_mean", "CCA2_sd")


category_centroids_trim <- category_centroids[,-4]

category_centroids_trim <- category_centroids_trim[c(-1,-2,-6,-7,-12,-15,-16,-21,-20,-25,-27,-32,-31,-26),]
category_centroids_trim <- as.data.frame(category_centroids_trim, stringsAsFactors=F)

category_centroids_trim <- as.data.frame(replace(as.matrix(category_centroids_trim),
                                                 category_centroids_trim == "chem", "chemistry"))
category_centroids_trim <- as.data.frame(replace(as.matrix(category_centroids_trim),
                                                 category_centroids_trim == "fossilfuel", "fossil fuel"))


centroid_names <- category_centroids_trim$category

category_means <- as.matrix(cbind(as.numeric(as.character(category_centroids_trim$CCA1_mean)),
                                  as.numeric(as.character(category_centroids_trim$CCA2_mean))))



nrow(category_means)
category_means[4, 1] = 0.56462222 #bird
category_means[18, 1] = 0.67279153 #reptile
category_means[10, 1] = -0.633019837 #evidence
category_means[17, 1] = -0.363934676 #pollution


####Extract Species Correlations

#cor(PCACorr.mat, use= "all.obs", method= "pearson" )

#cols <- c(1:7)
####Plot CCA
jpeg("Word_CCA_topics.jpg", width = 12, height = 5, units = "in", pointsize = 12,
     quality = 200, bg = "white", res= 600)
par(mfrow=c(1,2), mar=c(4.2, 4.2, .5, .5))#it goes c(bottom, left, top, right)

#_______________

plot(ord, type = "n", main = "", xlab = "Axis 1 (11.3%)", ylab = "Axis 2 (9.0%)", 
     xlim=c(-1,4), ylim=c(-2,3))
abline(h=0, col="white")
abline(v=0, col="white")
box()
# Add points colored by Environmental Variable Management
cols <- c("darkorange4",
          "darkgoldenrod3", "olivedrab4")
points(ord, col = adjustcolor(cols[type],alpha.f = .7), pch = c(8,11,15)[as.numeric(type)], cex=.9)
ordiellipse(site_scores, type, kind = "se",
            conf=0.95, lwd=.75, draw = "polygon",col = cols, border= cols, alpha=30)
text(category_means, labels = centroid_names, font=3, cex=.55, srt=-40)
text(x=-1,y=2.75, labels = "a", font=1, cex=2)

# add legend
legend("topright", legend=levels(type), col=adjustcolor(cols,alpha.f = .7), pch = c(8,11,15), bty="n", cex=.9)



##by agency
plot(ord, type = "n", main = "", xlab = "Axis 1 (11.3%)", ylab = "", 
     xlim=c(-1,4), ylim=c(-2,3))
abline(h=0, col="white")
abline(v=0, col="white")
box()
# Add points colored by Environmental Variable Management
#cols <- c("grey45","gray90","coral3","gray65","darkorange3","hotpink4",
#          "gray30","darkgoldenrod3", "olivedrab4", "dodgerblue4", "steelblue")
cols <- c("coral3","darkorange4","hotpink4",
          "darkgoldenrod3", "olivedrab4", "dodgerblue2", "steelblue4")
points(ord, col = adjustcolor(cols[agency],alpha.f = .7), pch = c(8,11,15,16,17,23,25)[as.numeric(agency)], cex=.9)
ordiellipse(site_scores[c(-1,-8),], subset(agency, agency != "Badlands NP"), kind = "se",
            conf=0.95, lwd=.75, draw = "polygon",col = cols, border= cols, alpha=30)
text(category_means, labels = centroid_names, font=3, cex=.55, srt=-40)
text(x=-1,y=2.75, labels = "b", font=1, cex=2)

# add legend
legend("topright", legend=levels(agency), col=adjustcolor(cols[agency],alpha.f = .7), pch = c(8,11,15,16,17,23,25)[as.numeric(agency)], bty="n", cex=.9)


dev.off()

####Greyscale Ordinations####
jpeg("Word_CCA_topics_grey.jpg", width = 12, height = 5, units = "in", pointsize = 12,
     quality = 200, bg = "white", res= 600)
par(mfrow=c(1,2), mar=c(4.2, 4.2, .5, .5))#it goes c(bottom, left, top, right)

#_______________

plot(ord, type = "n", main = "", xlab = "Axis 1 (11.3%)", ylab = "Axis 2 (9.0%)", 
     xlim=c(-1,4), ylim=c(-2,3))
abline(h=0, col="white")
abline(v=0, col="white")
box()
# Add points colored by Environmental Variable Management
cols <- gray.colors(3,  start = 0.05, end = 0.6)
points(ord, col = "black", pch = c(8,11,15)[as.numeric(type)], cex=.9)
ordiellipse(site_scores, type, kind = "se",
            conf=0.95, lwd=.75, draw = "polygon",col = cols, border= cols, alpha=30)
text(category_means, labels = centroid_names, font=3, cex=.55, srt=-40)
text(x=-1,y=2.75, labels = "a", font=1, cex=2)

# add legend
legend("topright", legend=levels(type), col="black", pch = c(8,11,15), bty="n", cex=.9)



##by agency
plot(ord, type = "n", main = "", xlab = "Axis 1 (11.3%)", ylab = "", 
     xlim=c(-1,4), ylim=c(-2,3))
abline(h=0, col="white")
abline(v=0, col="white")
box()
# Add points colored by Environmental Variable Management
#cols <- c("grey45","gray90","coral3","gray65","darkorange3","hotpink4",
#          "gray30","darkgoldenrod3", "olivedrab4", "dodgerblue4", "steelblue")
cols <- gray.colors(7,  start = 0.05, end = 0.8)
points(ord, col = "black", pch = c(8,11,15,16,17,23,25)[as.numeric(agency)], cex=.9)
ordiellipse(site_scores[c(-1,-8),], subset(agency, agency != "Badlands NP"), kind = "se",
            conf=0.95, lwd=.75, draw = "polygon",col = cols, border= cols, alpha=30)
text(category_means, labels = centroid_names, font=3, cex=.55, srt=-40)
text(x=-1,y=2.75, labels = "b", font=1, cex=2)

# add legend
legend("topright", legend=levels(agency), col= "black", pch = c(8,11,15,16,17,23,25)[as.numeric(agency)], bty="n", cex=.9)


dev.off()