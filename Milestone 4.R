#first we want to remove any entries not under the "Normal" Category
sets1 <- subset(sets, Category == "Normal")
View(sets1)

#Then we remove any sets without Pieces as these are abnormal 
#find the rows for which na is in the Pieces column
no_pieces <- is.na(sets1$Pieces)

#remove these rows from data
sets2 <- subset(sets1, subset = !no_pieces)

#Now we want to set any minifigure count of NA to 0 as these will mess with minifigure analysis
sets2$Minifigures[is.na(sets2$Minifigures)] <- 0
sets_clean <- sets2

#for further use and cleaning we will create a subset with only the rows with a positive number of Minifigures
sets_clean_minifigures <- subset(sets_clean, Minifigures > 0)

#export as csv files to use later
write.csv(sets_clean, file = "MAT 782/Files/sets_clean.csv")
write.csv(sets_clean_minifigures, file = "MAT 782/Files/sets_clean_minifigures.csv")

#recreate old graphs with cleaned data
hist(sets_clean$Pieces, xlab = "Number of Pieces", ylab = "Frequency", main = "Sets Released per Size")
hist(sets_clean$Minifigures, xlab = "Number of Minifigures", ylab = "Frequency", main = "Sets Released per Number of Minifigures")
hist(sets_clean$USD_MSRP, xlab = "USD_MSRP", ylab = "Frequency", main = "Sets Released per Price")
barplot(table(sets_clean$Year), xlab = "Year", main = "Sets Released per Year", ylab =  "Number of Sets Released")
barplot(sort(table(sets_clean$Theme), decreasing = TRUE) [1:40], xlab = "Theme", ylab = "Number of Sets Released", main = "Sets Released per Theme", las = 2)
