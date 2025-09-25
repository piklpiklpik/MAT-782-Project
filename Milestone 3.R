#import and view dataset making any NA values empty
library(readr)
sets <- read_csv("MAT 782/Files/sets.csv")
View(sets)

#create histogram of Pieces column
hist(sets$Pieces, xlab = "Number of Pieces", ylab = "Frequency", main = "Sets Released per Size")

#create histogram of Minifigures column
hist(sets$Minifigures, xlab = "Number of Minifigures", ylab = "Frequency", main = "Sets Released per Number of Minifigures")

#create histogram of USD_MSRP column
hist(sets$USD_MSRP, xlab = "USD_MSRP", ylab = "Frequency", main = "Sets Released per Price")

#create barplot of Year column
barplot(table(sets$Year), xlab = "Year", main = "Sets Released per Year", ylab =  "Number of Sets Released")

#create barplot of Theme column only looking at the top 40 themes by frequency so it will fit on graph
barplot(sort(table(sets$Theme), decreasing = TRUE) [1:40], xlab = "Theme", ylab = "Number of Sets Released", main = "Sets Released per Theme", las = 2)
