library(readr)
sets_cleaner <- read_csv("MAT 782/Files/sets_cleaner.csv")
sets_cleaner_minifigures <- read_csv("MAT 782/Files/sets_cleaner_minifigures.csv")
View(sets_cleaner)
View(sets_cleaner_minifigures)

#create linear model of price vs size then plot w/ regression line and plot residuals
price_vs_size = lm(sets_cleaner$USD_MSRP ~ sets_cleaner$Pieces)
summary(price_vs_size)
plot(sets_cleaner$Pieces, sets_cleaner$USD_MSRP)
abline(price_vs_size, col ="red")
hist(resid(price_vs_size))
plot(sets_cleaner$Pieces, resid(price_vs_size))
abline(h=0)

#create linear model of price vs minis then plot w/ regression line and plot residuals
price_vs_mini = lm(sets_cleaner_minifigures$USD_MSRP ~ sets_cleaner_minifigures$Minifigures)
summary(price_vs_mini)
plot(sets_cleaner_minifigures$Minifigures, sets_cleaner_minifigures$USD_MSRP)
abline(price_vs_mini, col = "red")
hist(resid(price_vs_mini))
plot(sets_cleaner_minifigures$Minifigures, resid(price_vs_mini))
abline(h=0)

#create linear model of price vs minis $ size then plot w/ regression line and plot residuals
price_lm = lm(sets_cleaner_minifigures$USD_MSRP ~ sets_cleaner_minifigures$Minifigures + sets_cleaner_minifigures$Pieces)
summary(price_lm)
hist(resid(price_lm))
plot(sets_cleaner_minifigures$Pieces, resid(price_lm))
abline(h=0)
plot(sets_cleaner_minifigures$Minifigures, resid(price_lm))
abline(h=0)
