library(readr)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(tidyr)


# To import a matrix and not a dataframe use read.transactions
# To remove the duplicates withinthe same transaction use rm.duplicates = TRUE
df <- read.transactions(file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans.csv", 
                        sep = ",",
                        rm.duplicates=TRUE)

# To change the items with the category
kat <- read_csv("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/sku_prod.csv")
trans1<- read_csv("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans1.csv")

class(df)
mode(df)

# Inspect df 
# inspect (df) # You can view the transactions
inspect(head(df))
length (df) # Number of transactions.
size (df) # Number of items per transaction
LIST(df) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(df)# To see the item labels

# Visualize items within your dataset
fp<-itemFrequencyPlot(df)

# Visualize up to 125 possible items on the x axis in the dataset
image(df)

# Specify the numberr of transactions to view
image(sample(df, 1000))

# Run the Apriori Principle
# Receiving 0 rules means that I need to experiment with the Support and Confidence values. 
rules<- apriori (df, parameter = list(supp = 0.001, conf = 0.3))
rules

# Inspect the Rule findings
inspect(head(sort(rules, by="lift"),3))

#### Evaluate the Model ####

# View the rules metrics
summary(rules)

# Print Top 10 Rules by Support
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10))

# Print Top 10 Rules by Confidence
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 3))

#### CONVENTIONAL PLOTTING TO EXPORT IMG
plot(rules)

head(quality(rules))

plot(rules, 
     measure=c("support","lift"), 
     shading="confidence")

plot(rules, 
     shading="order", 
     control=list(main ="Two-key plot"))

plot(rules, 
     method="grouped")

plot(rules, 
     method="grouped", 
     control=list(k=50))

# Limit the number of Rules to an acceptible amount
sel = plot(rules, 
           measure=c("support","lift"), 
           shading="confidence")

subrules = rules[quality(rules)$confidence > 0.8]
subrules

# Subset the rules to the top 30 most important rules and then inspect the smaller set of rules individually 
# to determine where there are meaningful associations.

subrules2 = head(sort(rules, by="lift"), 30)
subrules2

# IMPROVE THE RULES BY SORTING TOP VALUES
top.lift <- sort(subrules2, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(subset(top.lift), 10))

# Inspect a specific Item
ItemRules <- subset(subrules2, items %in% "SPH0016")
inspect(ItemRules)

#### The Insightful Rules Category ####
#### The Irrelevant Rules Category ####
#### The Unclear Rules Category ####

plot(subrules2, method="graph")
plot(subrules2, method="graph", control=list(type="items"))

# Parallel coordinates
# This plot is useful when comparing many variables together and seeing the relationships between them.
# Each variable is given its own axis and all the axes are placed in parallel to each other.
# Each axis can have a different scale, as each variable works off a different unit of measurement or all the axes can be normalised to keep all the scales uniform.
# To avoid clustering, Brushing highlights a selected line or collection of lines while fading out all the others. 

plot(subrules, method="paracoord")

plot(subrules, method="paracoord", control=list(reorder=TRUE))
oneRule = sample(rules, 1)
inspect(oneRule)

#### APPLICATION TO VIEW THE RULES AND FAST TUNE THEM ####
arulesViz::ruleExplorer(rules)

#### FIND ANY REDUNDANT RULES ####
is.redundant(rules)
# in specific measure
is.redundant(rules, measure="confidence")
?is.redundant


# MY FIRST APPROACH USIG STACK. SEE Script03 for team decided solution.

sdf <- stack(trans)
names(sdf)<-c("sku","col")
head(sdf)

sdf<-as_tibble(sdf)

class(kat)

sdf %>% 
  select(sku) %>% 
  filter(sku=="APP0094")

kat %>% 
  select(sku) %>% 
  filter(sku=="APP0094")

category <- kat %>% 
  inner_join(sdf) %>%
  group_by(sku)

# Remove duplicate values from category. All grouped categories need to be 
uni<-category%>% 
  distinct() %>% 
  ungroup() %>%
  unite("def", sku:manual_categories, remove = FALSE) %>% 
  select(-sku,-manual_categories) %>% 
  filter()
  

# Transpose to previous matrix
# Create grouped id
grpID<-uni %>% 
  group_by(col) %>% 
  mutate(grouped_id = row_number())
grpID

# Now spread

grpID %>% 
  spread(col, def) %>% 
  select(-grouped_id)

