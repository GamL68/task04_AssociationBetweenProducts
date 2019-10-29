library(readr)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


# This is the output csv file that contains the id_order and the category
# dft <- read.transactions(file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans_categories.csv",
                                # format = "single", 
                                # sep = ",",
                                # header = T,
                                # cols = c(1,2),
                                # rm.duplicates = T,
                                # quote = "\n")

# This is the file with the sku_prod
kat <- read_csv("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/sku_prod.csv")
brands <- read_csv2("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/products_with_brands.csv")

#### Upload the dataframe into basket format,
# This is the transaction original file with the SKU
trans <- read.transactions(file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans.csv", sep = ",")

# View First rows in transaction file
# inspect(head(trans))


# TRANS transaction file is used for MBA

# length (trans) # Number of transactions.
# size (trans) # Number of items per transaction
# LIST(trans) # Lists the transactions by conversion (LIST must be capitalized)

# View labels within trans
# itemLabels(trans)

# trans@itemInfo$labels shows all the sku in the transaction file. These need to be renamed i.e to sku
# We perform a left_join on the kat file which contained the sku_prod information.
# The resulting df will show sku and manual_categories

#### Joining Transaction file with brands and kat files

# Join with kat df to get categories assigned to sku
  temp_kat<-trans@itemInfo %>% 
    rename(sku = labels) %>%
    left_join(y = kat, by = "sku")
  
# Join with brands file to add a brand column
  
  temp<-temp_kat %>%
    left_join(y=brands, by="sku")
  
# Combine Category and Brand
  temp$Brand_Kat<- paste(temp$brand, temp$categories)
  
# Keep only the unique values
  temp<-unique(temp)
  
# Rename Column
    names(temp)[names(temp) == "manual_categories"] <- "categories"
    
#### Replacing the Labels
    
# Replace Trans labels with temp Brands_Kat
    trans@itemInfo$labels <-temp$Brand_Kat
    
# Replace Trans labels with temp Categories
    # trans@itemInfo$labels <-temp$categories

# Replace Trans labels with temp Brands
    #trans@itemInfo$labels <-temp$brand

#### AGGREGATION ----------------------------------------
    
# Aggregating the same labels within the trans based on the Replaced category or brand
    
  # Aggregate 
    trans_categories<-aggregate(trans, by = trans@itemInfo$labels)
    
  # Plot the frequency with which the categories are present in trans_category
    itemFrequencyPlot(trans_categories, topN = 5)

#### GENERATE RULES ----------------------------------------
    
# Num 01 Rule
    
    trans_rules<- apriori (trans_categories, parameter = list(supp = 0.0005, conf = 0.5,minlen=2)) # 40 Rules
    trans_rules
  
    inspect(head(sort(trans_rules, by="lift")))
    summary(trans_rules)
  
  # Num 02 Rule
  # trans_rules1<- apriori (trans_categories, parameter = list(supp = 0.02, conf = 0.9,minlen=2)) # 47 Rules
  # trans_rules1
  # 
  # inspect(head(sort(trans_rules1, by="lift")))
  # summary(trans_rules1)
  
  # Assign a unique variable to the chosen Rule. tHIS Needs to be set manually
  myRule <- trans_rules

  # View Plots application
  # arulesViz::ruleExplorer(myRule)
  
  # Choosing a rule
  top.support <- sort(myRule, decreasing = TRUE, na.last = NA, by = "support")
  inspect(head(top.support, 10))
  
  subrules = myRule[quality(myRule)$confidence > 0.8]
  subrules # Set of 7 rules
  
  subrules2 = head(sort(myRule, by="lift"), 30)
  subrules2 # Set of 30 Rules
  
  r <- as(myRule, "data.frame") 
  r[order(r$lift, r$confidence), ]
  
  # Chosen subrule
  subRule<-subrules
  
  top.lift <- sort(subRule, decreasing = TRUE, na.last = NA, by = "lift")
  inspect(head(subset(top.lift),10))
  
  # Find Reduntant Rules
  is.redundant(myRule)
  is.redundant(myRule, measure="confidence")
  
  # Is Significant Rules
  is.significant(myRule, trans_categories)
  
  # Find maximal itemsets
  is.maximal(myRule)
  plot(myRule,  measure=c("support", "confidence"), shading="lift")
  
  # View specific Item
  # ItemRules <- subset(subrules2, items %in% "Apple") #Not working
  # inspect(ItemRules)
  
  plot(myRule)
  head(quality(myRule))

  #### CATEGORIES -------------------------------------------------------------------
  
  # This method can be kept when all the items are used without needing to filter out any specific criteria
  
  # Specific Items ------------------------------------------------------------------
  # Testing ACCESSORIES
  
  acc.rules <- sort(subset(myRule, subset = rhs %in% "accessories"), by = "confidence")
  summary(acc.rules)
  
  inspect(head(acc.rules))
  is.significant(acc.rules, trans_categories) 
  is.maximal(acc.rules) 
  is.redundant(acc.rules) 
  plot(acc.rules,  measure=c("support", "confidence"), shading="lift")
  
  is.superset(acc.rules)
  is.subset(acc.rules)
  
  # Testing PC
  
  pc.rules <- sort(subset(myRule, subset = rhs %in% "pc"), by = "confidence")
  summary(pc.rules)
  
  inspect(head(pc.rules))
  is.significant(pc.rules, trans_categories) 
  is.maximal(pc.rules) 
  is.redundant(pc.rules) 
  plot(pc.rules,  measure=c("support", "confidence"), shading="lift")
  
  is.superset(pc.rules)
  is.subset(pc.rules)
  
  # GROUPING ITEMS -------------------------------
  
  # Tablet, laptop or pc products
  grp.rules <- sort(subset(myRule, subset = lhs %in%  "pc"|lhs %in%  "laptop" |lhs %in%  "tablet"), by = "confidence")
  summary(grp.rules)
  
  inspect(head(grp.rules))
  is.significant(grp.rules, trans_categories) 
  is.maximal(grp.rules) 
  is.redundant(grp.rules) 
  plot(grp.rules,  measure=c("support", "confidence"), shading="lift")
  
  is.superset(grp.rules)
  is.subset(grp.rules)
  
  plot(grp.rules,method="graph",interactive=FALSE,shading="lift")
  title(main = "Tablet, PC, Laptop")
  
  # Jaccard Index ----------------------------
  
  # See how much likely two items are to be bought together.
  
  trans.sel<-trans_categories[,itemFrequency(trans_categories)>0.1] # selected transactions
  dissimilarity(trans.sel, which="items") 
  
  library(colorspace)
  
  # Matrix and Grouped Plots ----------------------------
  plot(grp.rules, method="matrix", measure=c("support","confidence"), control=list(col=sequential_hcl(200)))
  plot(grp.rules, method="grouped", measure="support", control=list(col=sequential_hcl(100)))
  
  # parallel coordinates plot ----------------------------
  # We can show dependencies with parallel coordinates plot. 
  # We can see that the mostly red arrow (each of them represents one rule) connects pc with extended warrenty. 
  # Moreover most of the arrows connect sausage on the first position, as previously stated.
  
  plot(grp.rules, method="paracoord", control=list(reorder=TRUE))
  
  
  # Tree Map ----------------------------
  
  # Shows us how many products of each type are available to buy within the computer store. 
  
  # Each of these charts have different level of depth. 
  # First only shows the bigger group names (like aisles in the shop). 
  # Second shows deeper segmentation intro product types (for example within aisle). 
  # The last chart presents each of the products available - it does give us less information that the previous one.
  
  occur1 <- trans_categories@itemInfo %>% group_by(labels.1) %>% summarize(n=n())
  # occur2 <- trans_categories@itemInfo %>% group_by(labels.1, level2) %>% summarize(n=n())
  # occur3 <- trans_categories@itemInfo %>% group_by(labels.1, labels) %>% summarize(n=n())
  
  library(treemap)
  treemap(occur1,index=c("labels.1"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")
  # treemap(occur3,index=c("labels.1, labels"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")
  
  
  # Inspect less likely ----------------------------
  
  # Items that are less than likely to be bought together. These would be described by lift < 1.
  inspect(tail(sort(myRule, by = "lift")))
  
  #### BRAND -------------------------------------------------------------------
  
  # Testing Apple
  apple.rules <- sort(subset(myRule, subset = rhs %in% "Apple"), by = "confidence")
  summary(apple.rules)
  
  inspect(head(apple.rules))
  is.significant(apple.rules, trans_categories) 
  is.maximal(apple.rules) 
  is.redundant(apple.rules) 
  plot(apple.rules,  measure=c("support", "confidence"), shading="lift")
  
  
  # Jaccard Index For Brands
  
  # See how much likely two items are to be bought together.
  trans.selB<-trans_categories[,itemFrequency(trans_categories)>0.1] # selected transactions
  dissimilarity(trans.selB, which="items") 
  
  library(colorspace)
  # Advanced Graphics
  plot(apple.rules, method="matrix", measure=c("support","confidence"), control=list(col=sequential_hcl(200)))
  plot(apple.rules, method="grouped", measure="support", control=list(col=sequential_hcl(100)))
  
  
  # We can show dependencies with parallel coordinates plot. 
  # We can see that the mostly red arrow (each of them represents one rule) connects pc with extended warrenty. 
  # Moreover most of the arrows connect sausage on the first position, as previously stated.
  
  
  plot(apple.rules, method="paracoord", control=list(reorder=TRUE))
  
  
  #### Tree Map ####
  # Shows us how many products of each type are available to buy within the computer store. 
  
  # Each of these charts have different level of deepth. 
  # First only shows the bigger group names (like aisles in the shop). 
  # Second shows deeper segmentation intro product types (for example within aisle). 
  # The last chart presents each of the products available - it does give us less information that the previous one.
  
  occur1 <- trans_categories@itemInfo %>% group_by(labels.1) %>% summarize(n=n())
  # occur2 <- trans_categories@itemInfo %>% group_by(labels.1, level2) %>% summarize(n=n())
  # occur3 <- trans_categories@itemInfo %>% group_by(labels.1, labels) %>% summarize(n=n())
  
  library(treemap)
  treemap(occur1,index=c("labels.1"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")
  # treemap(occur3,index=c("labels.1, labels"),vSize="n",title="",palette="Dark2",border.col="#FFFFFF")
  
  # Inspect less likely
  # Items that are less than likely to be bought together. These would be described by lift < 1.
  inspect(tail(sort(myRule, by = "lift")))
  
  
  #### Transform Rules into DataFrame
  # To filter out items within the transaction basket I need to transform it into a dataframe. 
  # I First find the rules with Apriori.
  BMA_df <- DATAFRAME(trans_rules, setStart = '', itemSep = ' + ', setEnd = '')
  head(BMA_df)  

# Filter LHS with APPLE products
  require(data.table)
  app_df<-BMA_df %>% 
            filter(LHS %like% 'Apple ')

  app_df1<-BMA_df %>% 
    filter(RHS %like% 'Apple ')
  
  # RHS vs LHS
  
  #app_df The filter is on the LHS items, at least 1 needs to be Apple
  
  basic <- ggplot( app_df , 
                   aes(x=RHS, y=LHS, size=confidence)) + 
            geom_point(color="#69b3a2") +
            theme_minimal() +
    ggtitle("RHS vs LHS")
  basic

  basic + 
    theme(axis.text.x = element_text( 
      angle = 90, 
      color="#4682B4", 
      size=8, 
      face=3)
    ) +
    theme(axis.text.y = element_text( 
      angle = 0, 
      color="#4682B4", 
      size=8, 
      face=3)
    )
  
  # app_df1 The filter is on the RHS products
  
  basic1 <- ggplot( app_df1 , 
                   aes(x=RHS, y=LHS, size=confidence)) + 
    geom_point(color="#69b3a2") +
    theme_minimal() +
    ggtitle("RHS vs LHS")
  basic1
  
  basic1 + 
    theme(axis.text.x = element_text( 
      angle = 90, 
      color="#4682B4", 
      size=8, 
      face=3)
    ) +
    theme(axis.text.y = element_text( 
      angle = 0, 
      color="#4682B4", 
      size=8, 
      face=3)
    )
  
  #### Make new CSV File with Brands, Kat and SKU
  
  newdata<-brands %>% 
    left_join(y = kat, by = "sku") %>% 
    filter(brand == "Apple")
  
  
  