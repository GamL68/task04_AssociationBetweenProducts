library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(lubridate)

#

itm_df <- read_csv2("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/lineitems.csv")
ord_df <- read_csv2("D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/orders_translated.csv")
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

# check duplicate rows based onstate, total and Date
ord_df1[duplicated(ord_df1 [c(3,4,5)]),]

# 
ord_df2<-ord_df1 %>%
  group_by(yDate) %>%
  count(yDate, sort = TRUE) %>% 
  arrange(desc(yDate))

ggplot(filter(ord_df1),
       aes(x=total_paid))+
  geom_histogram()

