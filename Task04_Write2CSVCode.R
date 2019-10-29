library(stringr)

temp_brands <- str_sub(temp$sku, start = 0, end = 3)

temp_brands <- as.data.frame(unique(temp_brands))
write.csv(temp_brands, file = "D:/UBIQUM/GITHUB/Task04_AssociationBetweenProducts/sku_brands.csv")

length(unique(temp_brands))
