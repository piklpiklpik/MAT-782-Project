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