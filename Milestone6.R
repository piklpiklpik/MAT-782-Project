#create scatterplot of Pieces Vs Price from our adjusted and cleaned data
plot(sets_cleaner$USD_MSRP, sets_cleaner$Pieces, main = "Piece Count Vs USD_MSRP", ylab = "Piece Count", xlab = "USD MSRP")

#calculate correlation of pieces vs price (0.8655536)
cor(sets_cleaner$USD_MSRP, sets_cleaner$Pieces)

#create scatterplot of Minifigures vs price
plot(sets_cleaner_minifigures$USD_MSRP, sets_cleaner_minifigures$Minifigures, main = "Minifigure Count Vs USD_MSRP", xlab = "USD MSRP", ylab = "Minifigure Count")

#calculate correlation of minifigures vs price (0.5632504)
cor(sets_cleaner_minifigures$USD_MSRP, sets_cleaner_minifigures$Minifigures)


