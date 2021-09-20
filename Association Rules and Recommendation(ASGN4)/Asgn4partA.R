#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
## read demo data 
tr <- read.transactions("/media/yomna/New\ Volume/DEBI/uOttawa/DS/Asgn4/transactions.csv", format = "basket", sep=",", skip = 0,  rm.duplicates=FALSE)
inspect(tr)
## make always sure that the items were properly separated
itemLabels(tr)

# plot the frequency of items
itemFrequencyPlot(tr)
library(RColorBrewer)
itemFrequencyPlot(tr, topN = 10,col=brewer.pal(8,'Pastel2'),main="Item Frequency Plot")
itemFrequencyPlot(tr, type = 'absolute' , topN = 10,col=brewer.pal(8,'Pastel2'),main="Item Frequency Plot")

summary (tr)
# ------------------------


# default settings result in zero rules learned
apriori(tr)

# set better support and confidence levels to learn more rules
tr_rules <- apriori(tr, parameter = list(support =0.002, confidence = 0.2, maxlen = 3))
tr_rules
tr_rules_by_lift <- sort(tr_rules, by = "lift", decreasing=TRUE)
inspect(head(tr_rules_by_lift))
tr_rules_by_lift_1 = tr_rules_by_lift[1]
inspect(tr_rules_by_lift_1)
# ------------------------
tr_rules_2 <- apriori(tr, parameter = list(support =0.002, confidence = 0.2, maxlen = 2))
tr_rules_2_by_lift <- sort(tr_rules_2, by = "lift")
inspect(head(tr_rules_2_by_lift))
tr_rules_2_by_lift_1 = tr_rules_2_by_lift[1]
inspect(tr_rules_2_by_lift_1)

#---------------------------------------