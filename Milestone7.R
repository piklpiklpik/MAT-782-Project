#finding 95% confidence intervals for price, size, and minifigure count

#first compute sample means
#price mean = 34.04467
msrp_mean <- mean(sets_cleaner$USD_MSRP)

#price mean w/ minifigure>0 = 8.969047
msrp_mean_mini <- mean(sets_cleaner_minifigures$USD_MSRP)

#size mean = 390.3782
size_mean <- mean(sets_cleaner$Pieces)

#size mean w/ minifigures>0 = 379.5254
size_mean_mini <- mean(sets_cleaner_minifigures$Pieces)

#mini mean = 2.144158
mini_mean <- mean(sets_cleaner$Minifigures)

#mini mean w/ minis>0 = 3.171682
mini_mean_mini <- mean(sets_cleaner_minifigures$Minifigures)

#next compute sd of each

#price sd = 3.47509
msrp_sd <- sd(sets_cleaner$USD_MSRP)

#price sd w/ mini>0 = 0.8078311
msrp_sd_mini <- sd(sets_cleaner_minifigures$USD_MSRP)

#size sd = 657.5828
size_sd <- sd(sets_cleaner$Pieces)

#size sd w/ mini>0 = 552.2408
size_sd_mini <- sd(sets_cleaner_minifigures$Pieces)

#mini sd = 2.632046
mini_sd <- sd(sets_cleaner$Minifigures)

#mini sd w/ mini>0 = 2.643592
mini_sd_mini <- sd(sets_cleaner_minifigures$Minifigures)

#calculate the 95th percentile a/2 = 1.959964
qnorm(0.975)

#95% confidence interval = [mean-(a/2)*(sd), mean+(a/2)*(sd)]

#interval for size = [-898.4603, 1679.217]
size_mean + size_sd*(-qnorm(0.975))
size_mean - size_sd*(-qnorm(0.975))

#interval for size w/ mini>0 = [-702.8468, 1461.897]
size_mean_mini + size_sd_mini*(-qnorm(0.975))
size_mean_mini - size_sd_mini*(-qnorm(0.975))

#interval for price = [27.23362, 40.85573]
msrp_mean + msrp_sd*(-qnorm(0.975))
msrp_mean - msrp_sd*(-qnorm(0.975))

#interval for price w/ mini>0 = [7.385727, 10.55237]
msrp_mean_mini + msrp_sd_mini*(-qnorm(0.975))
msrp_mean_mini - msrp_sd_mini*(-qnorm(0.975))

#interval for mini = [-3.014556, 7.302873]
mini_mean + mini_sd*(-qnorm(0.975))
mini_mean - mini_sd*(-qnorm(0.975))

#interval for miniw/mini>0 = [-2.009663, 8.353028]
mini_mean_mini + mini_sd_mini*(-qnorm(0.975))
mini_mean_mini - mini_sd_mini*(-qnorm(0.975))

