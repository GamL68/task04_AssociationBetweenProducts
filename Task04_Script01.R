library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(lubridate)

#

itm_df <- read.delim("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/lineitems1.csv", sep = ";")
ord_df <- read.delim("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/orders_translated.csv", sep=";")
trns_df<-read_csv("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/trans1.csv")

#### CHECK NAs
anyNA(itm_df)
anyNA(ord_df)
anyNA(trns_df)

# Address NAs in ord_df ... shows that there are 5 NAs in the column total_paid
md.pattern(ord_df,plot = TRUE, rotate.names = TRUE)
md.pattern(itm_df,plot = TRUE, rotate.names = TRUE)
md.pattern(trns_df,plot = TRUE, rotate.names = TRUE)

# Filters out 
dplyr::filter(ord_df,is.na(total_paid))

ord_df %>%
  count(state, sort = TRUE)

# Round by month, group by date, filter only pending, count the total instances
ord_df_Date<-ord_df %>%
  mutate(yDate = date(created_date))

head(ord_df_Date)

ord_df_Date %>%
  group_by(yDate) %>%
  count(state, sort = TRUE) %>% 
  filter(state == "Pending")%>% 
  arrange(desc(yDate))

# Within the month of November 2017 there is a total of 536 Pending issues, 
# hence the 3 NA will be omitted.

# Create New obj without 5 NAs
ord_df1<-ord_df_Date %>% 
  na.omit(total_paid)

ord_df1 %>%
  group_by(state) %>%
  count(state, sort = TRUE)

ord_df1 %>%
  group_by(total_paid) %>%
  count(total_paid, sort = TRUE)

# check duplicate rows based on state, total and Date
# ord_df1[duplicated(ord_df1 [c(3,4,5)]),]

# 
ord_df2<-ord_df1 %>%
  group_by(yDate) %>%
  count(yDate, sort = TRUE) %>% 
  arrange(desc(yDate))

#### FILTER OUT ONLY COMPLETE ORDERS. Total Orders found 46605
ord_Completed<-ord_df1 %>%
  filter(state == "Completed") 

#### Create Join Table

# jt<-ord_Completed %>%
#   group_by(id_order) %>%
#   summarise(n = n_distinct(id_order)) %>% 
#   arrange(desc(id_order))

#### 
# itm_df%>%
#   group_by(id_order) %>%
#   summarise(n=n_distinct(id_order)) %>% 
#   arrange(desc(n))

#### JOIN TABLES ORDERS & LINEITEM

# jt_itm<-left_join(x=jt %>% select(id_order), y=itm_df, by="id_order")
# jt_itm

#### Transposing Transactions all columns into one

# inprogress<-gather(trns_df,Col_Num,sku,Items_01:Items_11)
# trns<-inprogress %>% drop_na()

#### JOINS

# Filter all items of Completed orders, where num of items is greater than 1
ord_df1 %>%
  filter(state == "Completed") %>%
  left_join(itm_df %>% select(id_order, product_quantity)) %>% # find out product quanity
  group_by(id_order) %>%
  summarise(n = n()) %>% # find out total quantity by order id
  filter(n > 1) %>% # take out all the orders with only 1 product
  nrow() # show me the total quanity of observations

# Filter all items whose transactions are completed and select those that have bought 
# more than one item.

trans_id <- ord_df1 %>%
  filter(state == "Completed") %>%
  left_join(itm_df %>% select(id_order, product_quantity)) %>% # find out product quanity
  group_by(id_order) %>%
  summarise(n = n()) %>% # find out total quantity by order id
  filter(n > 1)
  nrow(trans_id) # show me the total quanity of observations

# bind the id order from the item df with the transactions to assign an id to the table
  
ord_trns<-trans_id %>%
            bind_cols(trns_df)

md.pattern(ord_trns,plot = TRUE, rotate.names = TRUE)

Tot_ord_trns<-ord_df1 %>%
  filter(state == "Completed") %>%
  left_join(itm_df %>% select(id_order, product_quantity, unit_price)) %>% # find out product quanity
  group_by(id_order) %>% 
  filter(product_quantity > 1)

nrow(Tot_ord_trns)

md.pattern(Tot_ord_trns,plot = TRUE, rotate.names = TRUE)





























