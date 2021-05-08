### Kruskal-Wallis by links/link type for AltUSEPA ####
library(pgirmess)
tweets <- read.csv("AltUSEPA_link_summary.csv")

#doesn't show all comparisons...
test <- kruskal.test(retweets ~ type, data = tweets)
test <- kruskalmc(retweets ~ type, probs = 0.05, cont='one-tailed', data = tweets)

#This is much better:
library(PMCMR)

rt_test <- posthoc.kruskal.conover.test(retweets ~ type, data = tweets)
favs_test <- posthoc.kruskal.conover.test(favorites ~ type, data = tweets)




