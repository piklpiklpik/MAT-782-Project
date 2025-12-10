# first we will perform anova analysis on year data vs price

#first separate each year out using factor
sets_cleaner$Year <- as.factor(sets_cleaner$Year)
year_aov <- aov(USD_MSRP ~ Year, data = sets_cleaner)
summary(year_aov)

#visualize with boxplot
boxplot(USD_MSRP ~ Year, data = sets_cleaner)

#use Tukey to get better visualization
tukey_year <- TukeyHSD(year_aov, conf.level = 0.95)
plot(tukey_year)
#try something more accurate
plot(TukeyHSD(year_aov, conf.level = 0.99))

#not useful constrain to fewer years
tukey <- subset(sets_cleaner, sets_cleaner$Year == 1991 | sets_cleaner$Year == 2000 | sets_cleaner$Year == 2010 | sets_cleaner$Year == 2020)
tukey_aov <- aov(USD_MSRP ~ Year, data = tukey)
summary(tukey_aov)
boxplot(USD_MSRP ~ Year, data = tukey)
plot(TukeyHSD(tukey_aov, conf.level = 0.95))
#better but lets try for something more accurate
plot(TukeyHSD(tukey_aov, conf.level = 0.99))

#let's try every five years to see if its still understandable
tukey1 <- subset(sets_cleaner, sets_cleaner$Year == 1991 | sets_cleaner$Year == 1995 | sets_cleaner$Year == 2000 | sets_cleaner$Year == 2005 | sets_cleaner$Year == 2010 | sets_cleaner$Year == 2015 | sets_cleaner$Year == 2020)
tukey1_aov <- aov(USD_MSRP ~ Year, data = tukey1)
plot(TukeyHSD(tukey1_aov, conf.level = 0.95))
#not totally readable so stay with 10 yr one

#try to reassign the data values to group them in 10 yr increments so we dont lose data
regroup <- subset(sets_cleaner)
regroup$Year[regroup$Year == 1992 | regroup$Year ==1993] <- 1991
regroup$Year[regroup$Year == 1996 | regroup$Year == 1997 | regroup$Year == 1998 | regroup$Year == 1999 | regroup$Year == 2001 | regroup$Year == 2002 | regroup$Year == 2003 | regroup$Year == 2004 | regroup$Year ==2005] <- 2000
regroup$Year[regroup$Year == 2006 | regroup$Year == 2007 | regroup$Year == 2008 | regroup$Year == 2009 | regroup$Year == 2011 | regroup$Year == 2012 | regroup$Year == 2013 | regroup$Year == 2014 | regroup$Year == 2015] <- 2010
regroup$Year[regroup$Year == 2016 | regroup$Year == 2017 | regroup$Year == 2018 | regroup$Year == 2019 | regroup$Year == 2021 | regroup$Year == 2022 | regroup$Year == 2023] <- 2020
regroup_aov <- aov(USD_MSRP ~ Year, data = regroup)
summary(regroup_aov)
boxplot(USD_MSRP ~ Year, data = regroup)
plot(TukeyHSD(regroup_aov, conf.level = 0.95))
#this seems to be the most accurate yet but still constrained by low data amounts in the earlier years

#now try again but this time compare to set size as this is our strongest determiner of set price
regroup_aov1 <- aov(Pieces ~ Year, data = regroup)
summary(regroup_aov1)
boxplot(Pieces ~ Year, data = regroup)
plot(TukeyHSD(regroup_aov1, conf.level = 0.95))
#agrees with other aov if a bit stronger

#try to do it with theme to see how it goes
sets_cleaner$Theme <- as.factor(sets_cleaner$Theme)
theme_aov <- aov(USD_MSRP ~ Theme, data = sets_cleaner)
summary(theme_aov)
plot(TukeyHSD(theme_aov, conf.level = 0.95))
#unuseful and cannot group together like year.

#should instead use the multi mean test for set size vs mean as before in milestone 9

themes <- unique(sets_cleaner$Theme)
test_themes <- list()
for(i in themes){
  theme <- subset(sets_cleaner$Pieces, sets_cleaner$Theme == i)
  if(mean(theme) > mean(sets_cleaner$Pieces)){
    print(i)
    print(mean(theme))
    test_themes <- append(test_themes, i)
  }
}

saveRDS(test_themes, file = "MAT 782/Files/test_themes_Pieces.rds")

#Star Wars passed w/ mean = 561.9824 and p = 1.352e-06
test <- subset(sets_cleaner, sets_cleaner$Theme == "Star Wars")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Technic passed w/ mean = 844.0198 and p = 5.471e-13
test <- subset(sets_cleaner, sets_cleaner$Theme == "Technic")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Vikings error cause only one set
test <- subset(sets_cleaner, sets_cleaner$Theme == "Vikings")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Batman failed w/ mean 472.6667 and p = 0.2351
test <- subset(sets_cleaner, sets_cleaner$Theme == "Batman")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Advanced models passed w/ mean = 1887.926 and p = 3.524e-07
test <- subset(sets_cleaner, sets_cleaner$Theme == "Advanced models")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Harry Potter passed w/ mean = 765.6491 and p = 0.01082
test <- subset(sets_cleaner, sets_cleaner$Theme == "Harry Potter")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Indiana Jones failed w/ mean = 444.4737 and  p = 0.2531
test <- subset(sets_cleaner, sets_cleaner$Theme == "Indiana Jones")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Agents failed w/ mean = 433.7692 and p = 0.3055
test <- subset(sets_cleaner, sets_cleaner$Theme == "Agents")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Factory passed w/ mean = 914.6667 and p = 0.0007757
test <- subset(sets_cleaner, sets_cleaner$Theme == "Factory")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Architecture passed w/ mean = 651.7647 and p = 0.0003229
test <- subset(sets_cleaner, sets_cleaner$Theme == "Architecture")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#World Racers failed w/ mean = 392.5 and p = 0.4938
test <- subset(sets_cleaner, sets_cleaner$Theme == "World Racers")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Serious Play failed w/ mean 2559.5 and p = 0.2621
test <- subset(sets_cleaner, sets_cleaner$Theme == "Serious Play")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Ninjago failed w/ mean = 400.078 and p = 0.3924
test <- subset(sets_cleaner, sets_cleaner$Theme == "Ninjago")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Pirates of the Caribbean failed w/ mean 642.1 and p = 0.132
test <- subset(sets_cleaner, sets_cleaner$Theme == "Pirates of the Caribbean")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Master Builder Academy failed w/ mean = 490 and p = 0.1718
test <- subset(sets_cleaner, sets_cleaner$Theme == "Master Builder Academy")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#DC Comics Super Heroes failed w/ mean 446.6944 and p = 0.2984
test <- subset(sets_cleaner, sets_cleaner$Theme == "DC Comics Super Heroes")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Monster Fighters failed w/ mean = 594.2222 and p = 0.1769
test <- subset(sets_cleaner, sets_cleaner$Theme == "Monster Fighters")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The Lord of the Rings failed w/ mean = 635.9167 and p = 0.1099
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Lord of the Rings")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Promotional failed w/ mean = 517.8 and p = 0.2976
test <- subset(sets_cleaner, sets_cleaner$Theme == "Promotional")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Ideas passed w/ mean = 1279.913 and p = 3.732e-08
test <- subset(sets_cleaner, sets_cleaner$Theme == "Ideas")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The Hobbit failed w/ mean = 438.7143 and p = 0.2483
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Hobbit")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Creator Expert passed w/ mean = 2022.114 and p = 1.411e-09
test <- subset(sets_cleaner, sets_cleaner$Theme == "Creator Expert")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Minecraft failed w/ mean = 451.0233 and p = 0.07057
test <- subset(sets_cleaner, sets_cleaner$Theme == "Minecraft")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Teenage Mutant Ninja Turtles failed w/ mean = 411 and p = 0.3856
test <- subset(sets_cleaner, sets_cleaner$Theme == "Teenage Mutant Ninja Turtles")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The Lone Ranger failed w/ mean = 406.5 and p = 0.4448
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Lone Ranger")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Miscellaneous failed w/ mean = 582.4 and p = 0.2618
test <- subset(sets_cleaner, sets_cleaner$Theme == "Miscellaneous")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Ultra Agents failed w/ eman = 452.3571 and p = 0.2485
test <- subset(sets_cleaner, sets_cleaner$Theme == "Ultra Agents")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The LEGO Movie failed w/ mean = 554.4 and p = 0.1089
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Movie")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The Simpsons passed w/ mean = 2351 and p = 0.02785
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Simpsons")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Classic failed w/ mean = 485.9643 and p = 0.08288
test <- subset(sets_cleaner, sets_cleaner$Theme == "Classic")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Elves failed w/ mean = 416.6061 and p = 0.2777
test <- subset(sets_cleaner, sets_cleaner$Theme == "Elves")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Jurassic World failed w/ mean = 495.6897 and p = 0.1634
test <- subset(sets_cleaner, sets_cleaner$Theme == "Jurassic World")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Boost error only 1 set
test <- subset(sets_cleaner, sets_cleaner$Theme == "Boost")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The LEGO Batman Movie failed w/ mean = 618.48 and p = 0.06246
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Batman Movie")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The LEGO Ninjago Movie passed w/ mean = 1091.737 and p = 0.01161
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Ninjago Movie")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#The LEGO Movie 2 failed w/ mean = 541.4815 and p = 0.1268
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Movie 2")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Hidden Side failed w/ mean = 458.0476 and p = 0.1815
test <- subset(sets_cleaner, sets_cleaner$Theme == "Hidden Side")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Stranger Things error only 1 set
test <- subset(sets_cleaner, sets_cleaner$Theme == "Stranger Things")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Overwatch failed w/ mean = 392.375 and p = 0.4895
test <- subset(sets_cleaner, sets_cleaner$Theme == "Overwatch")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Icons passed w/ mean = 2596.475 and p = 6.023e-07
test <- subset(sets_cleaner, sets_cleaner$Theme == "Icons")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Super Mario failed w/ mean = 471.7895 and p = 0.1504
test <- subset(sets_cleaner, sets_cleaner$Theme == "Super Mario")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Art passed w/ mean = 3836.692 and p = 0.0001518
test <- subset(sets_cleaner, sets_cleaner$Theme == "Art")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Monkie Kid passed w/ mean = 900.9 and p = 0.0294
test <- subset(sets_cleaner, sets_cleaner$Theme == "Monkie Kid")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Avatar passed w/ mean = 612.3333 and p = 0.0331
test <- subset(sets_cleaner, sets_cleaner$Theme == "Avatar")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Dreamzzz passed w/ mean = 619.9091 and p = 0.04174
test <- subset(sets_cleaner, sets_cleaner$Theme == "Dreamzzz")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

#Sonic the Hedgehog failed w/ mean = 464.5 and p = 0.2818
test <- subset(sets_cleaner, sets_cleaner$Theme == "Sonic the Hedgehog")
t.test(x = test$Pieces, mu = mean(sets_cleaner$Pieces), alternative = "greater")

theme <- c("Star Wars","Technic","Advanced models","Harry Potter","Factory","Architecture","Ideas","Creator Expert","The Simpsons","The LEGO Ninjago Movie","Icons","Art","Monkie Kid","Avatar","Dreamzzz")
avg <- c(561.9824,844.0198,1887.926,765.6491,914.6667,651.7647,1279.913,2022.114,2351,1091.737,2596.475,3836.692,900.9,612.3333,619.9091)
large_themes <- data.frame(Theme = theme, Size = avg)
library(ggplot2)
ggplot(large_themes, aes(x = large_themes$Theme, y = large_themes$Size)) + geom_boxplot() + labs(title = "Average Size of Large Themes", x = "Theme", y = "Average Size")
















