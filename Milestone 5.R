#calculate the measures of center and spread of the discrete and continuous data fields

#Size
mean(sets_clean$Pieces)
mean(sets_clean$Pieces, trim = 0.05)
median(sets_clean$Pieces)
var(sets_clean$Pieces)
sd(sets_clean$Pieces)
# mean  = 238.5063
# trimmed mean = 166.7975
# median = 78
# variance = 232040.8
# sd = 481.7061

#Minifigures
mean(sets_clean$Minifigures)
mean(sets_clean$Minifigures, trim = 0.05)
median(sets_clean$Minifigures)
var(sets_clean$Minifigures)
sd(sets_clean$Minifigures)
# mean  = 1.575349
# trimmed mean = 1.257261
# median = 1
# variance = 5.591642
# sd = 2.364665

#Price
mean(sets_clean$USD_MSRP)
mean(sets_clean$USD_MSRP, trim = 0.05)
median(sets_clean$USD_MSRP)
var(sets_clean$USD_MSRP)
sd(sets_clean$USD_MSRP)
# mean  = 43.02793
# trimmed mean = 34.63584
# median = 24.99
# variance = 3270.735
# sd = 57.19034
