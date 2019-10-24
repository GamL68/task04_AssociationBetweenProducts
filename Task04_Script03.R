library(readr)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


# This is the output csv file that contains the id_order and the category
dft <- read.transactions(file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans_categories.csv",
                                format = "single", 
                                sep = ",",
                                header = T,
                                cols = c(1,2),
                                rm.duplicates = T,
                                quote = "\n")

# This is the file with the sku_prod
kat <- read_csv("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/sku_prod.csv")

# This is the transaction original file with the SKU
trans <- read.transactions(file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans.csv", sep = ",")

inspect(head(dft))
inspect(head(trans))
head(kat)

length (dft) # Number of transactions.
size (dft) # Number of items per transaction
LIST(dft) # Lists the transactions by conversion (LIST must be capitalized)

# View labels within trans
itemLabels(trans)

# dft@itemInfo$labels shows all the sku in the transaction file. These need to be renamed i.e to sku
# We perform a left_join on the kat file which contained the sku_prod information.
# The resulting df will show sku and manual_categories

temp<-trans@itemInfo %>% 
    rename(sku = labels) %>% 
    left_join(y = kat, by = "sku")

# Rename Column
    names(temp)[names(temp) == "manual_categories"] <- "categories"
    
  head(temp)

# Filter out only those labels that start with "AP" to identify the Apple products

trans@itemInfo$labels <-temp$categories
# Aggregate 
  trans_categories<-aggregate(trans, by = trans@itemInfo$labels)
  
  # Generate Rules
  # Num 01 Rule
  trans_rules<- apriori (trans_categories, parameter = list(supp = 0.02, conf = 0.9,minlen=2)) # 47 Rules
  trans_rules
  inspect(head(sort(trans_rules, by="lift")))
  summary(trans_rules)
  
  # Num 02 Rule
  trans_rules1<- apriori (trans_categories, parameter = list(supp = 0.018, conf = 0.9,minlen=2)) # 51870 Rules
  trans_rules1
  inspect(head(sort(trans_rules1, by="lift")))
  summary(trans_rules1)
  
  # Assign a unique variable to the chosen Rule. Needs to be set
  myRule <- trans_rules
  
  # View Plots
  arulesViz::ruleExplorer(myRule)
  
  # Choosing a rule
  top.support <- sort(myRule, decreasing = TRUE, na.last = NA, by = "support")
  inspect(head(top.support, 10))
  
  subrules = myRule[quality(myRule)$confidence > 0.8]
  subrules
  
  r <- as(myRule, "data.frame") 
  r[order(r$lift, r$confidence), ]
  
  # & lift > 1.4 & support > 0.4
  
  subrules2 = head(sort(myRule, by="lift"), 30)
  subrules2
  
  # Chosen subrule
  subRule<-subrules2
  
  top.lift <- sort(subRule, decreasing = TRUE, na.last = NA, by = "lift")
  inspect(head(subset(top.lift)[1:10]))
  
  
  
  # Find Reduntant Rules
  is.redundant(myRule)
  is.redundant(myRule, measure="confidence")
  
  
  # View specific SKU
  ItemRules <- subset(subrules2, items %in% "SPH0016")
  inspect(ItemRules)
  
  plot(myRule)
  head(quality(myRule))
  