#Author: George Obaido

#Get to know the packages
#'arules' package provides the infrastructure for representing, manipulating, and analyzing transaction data and patterns.
#'arulesviz' package is used for visualizing Association Rules and Frequent Itemsets.
# 'RColorBrewer' is a ColorBrewer Palette which provides color schemes for maps and other graphics.


#Importing Libraries
install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

# Loading Libraries
library(arules)
library(arulesViz)
library(RColorBrewer)

# import the Groceries dataset
#Groceries is a set of 9835 records/ transactions, each having 'n' number of items.
data(Groceries)
data() #Other datasets in arules package

#Get a summary of the Groceries dataset
summary(Groceries)

# using inspect() function to get the first 10 transactions
inspect(Groceries[1:10])

# using itemFrequencyPlot() function
#itemFrequencyPlot() creates a bar plot for item frequencies.
#Here, 'topN=20' means that 20 items with the highest item frequency/ lift will be plotted
#Read about: ??brewer.pal.info
arules::itemFrequencyPlot(Groceries, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "absolute", #use relative or absolute
                          ylab = "Item Frequency (Relative)")


#-----------------------Let's get down to the Apriori()--------------------------------------
# using apriori() function
rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001, conf = 0.8))

#Sort the rules
rules_conf <- sort(rules, by="confidence", decreasing=TRUE)

#Show the support, lift and confidence for all rules
inspect(head(rules_conf))

#Remove redundancies: Many rules tend to repeat!
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Let's show one item (whole milk) instead on the right hand side(rhs)
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])

#Let's show one item (whole milk) instead on the left hand side (lhs)
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])


#Plot the graph visualization
plot(rules,method="graph",interactive=TRUE,shading=NA)
