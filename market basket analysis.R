#Market Basket analysis_Electroindex
install.packages("arules")
install.packages("arulesViz")
update.packages("arulesViz")

library(arules)
library(arulesViz)
library(devtools)
install.packages("/Volumes/USB20FD/Big data/UT Data/Course 2/Task 4/arulesViz_1.3-0.tar", repos = NULL, type = "binary", lib="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")


#Upload dataset
setwd("~/Desktop/Austin DA/Course 2/Task 4")
??arules
??arulesviz
library(readr)
ElectronidexTransactions2017 <- read_csv("~/Desktop/Austin DA/Course 2/Task 4/ElectronidexTransactions2017.csv")
View(ElectronidexTransactions2017)
summary(ElectronidexTransactions2017)


#Transform dataset
ElectroData <- read.transactions(file = "ElectronidexTransactions2017", format = "basket", sep=",", rm.duplicates = TRUE)
View(ElectroData)


inspect (ElectroData) # You can view the transactions. Is there a way to see a certain # of transactions?
length (ElectroData) # Number of transactions.
size (ElectroData) # Number of items per transaction
LIST(ElectroData) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(ElectroData)# To see the item labels
summary(ElectroData)


#data visualization
?itemFrequencyPlot
itemFrequencyPlot(ElectroData)
itemFrequencyPlot(ElectroData)
itemFrequencyPlot(ElectroData, type="absolute", topN=25, horiz=TRUE, col='steelblue3', xlab='', main='item frequency, absolute')
image(sample(ElectroData, 50))

?barplot
barplot(sort(table(unlist(LIST(ElectroData))))[1:10], horiz = TRUE, las=2, col='steelblue3', xlab='', main='frequency, absolute')


?image
image(ElectroData)

#Apply Aprioru Rule
?apriori
?minlen
ElectroRules<- apriori(ElectroData, parameter = list(supp = 0.09, conf = 0.8, minlen = 2, maxlen=10))
summary(ElectroRules)
inspect(ElectroRules)
inspect(head(ElectroRules, by = "lift"))
inspect(sort(ElectroRules, by = "confidence"))
inspect(sort(ElectroRules, by = "support"))

#Interesting Rules 1
ElectroRules2<- apriori(ElectroData, parameter = list(supp = 0.09, conf = 0.7, minlen = 1, maxlen=10), appearance = list(rhs=c("iMac"), default = "lhs"))
inspect(ElectroRules2)
inspect(head(ElectroRules2, by = "lift"))
inspect(sort(ElectroRules2, by = "confidence"))
inspect(sort(ElectroRules2, by = "support"))

#interesting rules 2 
ElectroRules3<- apriori(ElectroData, parameter = list(supp = 0.09, conf = 0.7, minlen = 1, maxlen=10), appearance = list(rhs=c("iMac"), lhs=c("HP Laptop", "Apple Earpods", "Lenovo Desktop Computer", "Acer Aspire"), default = " none"))

#Transfer to excel 
install.packages("WriteXLS")
library(WriteXLS)
as(ElectroRules2, "data.frame")

Electroframe <- as.data.frame(ElectroRules2)
?write_csv
write.csv(ElectroRules2, file = "association rules.csv")

write(ElectroRules2, file = "association rule.csv", sep=",", quote = TRUE, row.names = FALSE)


#subset rules
?subset
ItemRules <- subset(ElectroRules2, subset = rhs %in% "iPad Pro" & lift > 2)
summary(ItemRules)

#Redundant Rules 
is.redundant(ElectroRules2)
rules.sorted <- sort(ElectroRules2, by = "lift")
inspect(rules.sorted)


# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
ElectroRedundant <- colSums(subset.matrix, na.rm=T) >= 1
which(ElectroRedundant)

#remove redundant rules
Electrorules.pruned <- rules.sorted[!ElectroRedundant]
inspect(Electrorules.pruned)
ElectroFinal <- Electrorules.pruned
summary(ElectroFinal)
inspect(ElectroFinal)

#Visualize rules
?plot
library(arulesViz)
library(plotly)
plot(ElectroRules2[1:5], method="graph", measure = "lift", control=list(type="items"))





