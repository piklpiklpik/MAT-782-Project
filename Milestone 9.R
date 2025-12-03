#conducting hypothesis tests to determine themes with higher mean prices

#first determine subsets that have higher means in our data

#separate out uniqe subsets
themes <- unique(sets_cleaner$Theme)

#loop through and if mean of subset is greater than overall mean then print out and add to list
test_themes <- list()
for(i in themes){
  theme <- subset(sets_cleaner$USD_MSRP, sets_cleaner$Theme == i)
  if(mean(theme) > mean(sets_cleaner$USD_MSRP)){
    print(i)
    print(mean(theme))
    test_themes <- append(test_themes, i)
  }
}

#download list of test themes
saveRDS(test_themes, file = "MAT 782/Files/test_themes.rds")

#create subset with only our test themes

#perform hypothesis tests to determine if the mean for each test theme is truly greater than our overall mean w/ alpha = 0.05
success <- data.frame(theme = character, mean = double())

#star wars pass w/ mean = 77.37085 and p = 3.222e-08
star_wars <- subset(sets_cleaner, sets_cleaner$Theme == "Star Wars")
t.test(x = star_wars$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#technic passed w/ mean = 103.387 and p = 3.599e-12
test <- subset(sets_cleaner, sets_cleaner$Theme == "Technic")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#castle failed w/ mean 52.62467 and p = 0.4166
test <- subset(sets_cleaner, sets_cleaner$Theme == "Castle")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")    

#Education passed w/ mean = 160.7821 and p = 3.023e-06
test <- subset(sets_cleaner, sets_cleaner$Theme == "Education")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Vikings (error cause only 1 vikings test)
test <- subset(sets_cleaner, sets_cleaner$Theme == "Vikings")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Batman failed w/ mean 61.45951 and p = 0.224
test <- subset(sets_cleaner, sets_cleaner$Theme == "Batman")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Mindstorms passed w/ mean = 117.1327 and p = 0.007035
test <- subset(sets_cleaner, sets_cleaner$Theme == "Mindstorms")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Advanced models passed w/ mean = 183.5224 and p = 3.092e-09
test <- subset(sets_cleaner, sets_cleaner$Theme == "Advanced models")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Harry Potter passed w/ mean 83.23399 and p = 0.01639
test <- subset(sets_cleaner, sets_cleaner$Theme == "Harry Potter")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Indiana Jones failed w/ mean 55.58681 and p = 0.3043
test <- subset(sets_cleaner, sets_cleaner$Theme == "Indiana Jones")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Agents failed w/ mean 57.74094 and p = 0.2539
test <- subset(sets_cleaner, sets_cleaner$Theme == "Agents")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Factory passed w/ mean = 123.8899 and p = 0.01696
test <- subset(sets_cleaner, sets_cleaner$Theme == "Factory")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Architecture passed w/ mean = 72.26624 and p = 0.001272
test <- subset(sets_cleaner, sets_cleaner$Theme == "Architecture")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#World Racers failed w/ mean = 55.01505 and p = 0.4057
test <- subset(sets_cleaner, sets_cleaner$Theme == "World Racers")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Serious Play failed w/ mean = 344.6838 and p = 0.2512
test <- subset(sets_cleaner, sets_cleaner$Theme == "Serious Play")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Pirates of the Caribbean failed w/ mean = 84.74728 and p = 0.08306
test <- subset(sets_cleaner, sets_cleaner$Theme == "Pirates of the Caribbean")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Master Builder Academy passed w/ mean = 80.75492 and p = 0.04008
test <- subset(sets_cleaner, sets_cleaner$Theme == "Master Builder Academy")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#DC Comics Super Heroes failed w/ mean = 55.3372 and p = 0.3423
test <- subset(sets_cleaner, sets_cleaner$Theme == "DC Comics Super Heroes")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Dino failed w/ mean = 58.27029 and p = 0.3236
test <- subset(sets_cleaner, sets_cleaner$Theme == "Dino")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Monster Fighters failed w/ mean = 74.91971 and p = 0.1706
test <- subset(sets_cleaner, sets_cleaner$Theme == "Monster Fighters")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The Lord of the Rings passed w/ mean = 82.24213 and p = 0.07728
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Lord of the Rings")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Ideas passed w/ mean = 125.5094 and p = 1.202e-06
test <- subset(sets_cleaner, sets_cleaner$Theme == "Ideas")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The Hobbit failed w/ mean = 63.56668 and p = 0.1441
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Hobbit")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Creator Expert passed w/ mean = 196.4867 and p = 9.242e-11
test <- subset(sets_cleaner, sets_cleaner$Theme == "Creator Expert")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Minecraft failed w/ mean = 53.00159 and p = 0.3469
test <- subset(sets_cleaner, sets_cleaner$Theme == "Minecraft")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Teenage Mutant Ninja Turtles failed w/ mean = 53.33639 and p = 0.39
test <- subset(sets_cleaner, sets_cleaner$Theme == "Teenage Mutant Ninja Turtles")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The Lone Ranger failed w/ mean = 59.32659 and p = 0.324
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Lone Ranger")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Ultra Agents failed w/ mean = 56.95789 and p = 0.2898
test <- subset(sets_cleaner, sets_cleaner$Theme == "Ultra Agents")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The LEGO Movie failed w/ mean = 62.03542 and p = 0.2312
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Movie")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The Simpsons passed w/ mean = 244.9668 and p = 0.003191
test <- subset(sets_cleaner, sets_cleaner$Theme == "The Simpsons")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Jurassic World passed w/ mean = 69.3625 and p = 0.04533
test <- subset(sets_cleaner, sets_cleaner$Theme == "Jurassic World")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Boost error cause only 1 boost set
test <- subset(sets_cleaner, sets_cleaner$Theme == "Boost")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The LEGO Batman Movie failed w/ mean = 70.8742 and p = 0.07783
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Batman Movie")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The LEGO Ninjago Movie passed w/ mean = 92.06118 and p = 0.0282
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Ninjago Movie")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#The LEGO Movie 2 failed w/ mean = 61.96159 and p = 0.2059
test <- subset(sets_cleaner, sets_cleaner$Theme == "The LEGO Movie 2")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Stranger Things error cause only 1 set
test <- subset(sets_cleaner, sets_cleaner$Theme == "Stranger Things")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Icons passed w/ mean = 219.1679 p = 2.544e-07
test <- subset(sets_cleaner, sets_cleaner$Theme == "Icons")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Super Mario failed w/ mean = 56.63291 and p = 0.2396
test <- subset(sets_cleaner, sets_cleaner$Theme == "Super Mario")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Art passed w/ mean = 137.0387 and p = 5.628e-06
test <- subset(sets_cleaner, sets_cleaner$Theme == "Art")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Monkie Kid failed w/ mean = 82.88722 and p = 0.06915
test <- subset(sets_cleaner, sets_cleaner$Theme == "Monkie Kid")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Avatar failed w/ mean = 75.13418 and p = 0.0604
test <- subset(sets_cleaner, sets_cleaner$Theme == "Avatar")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#DUPLO error cause only 1 set
test <- subset(sets_cleaner, sets_cleaner$Theme == "DUPLO")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Dreamzzz failed w/ mean = 63.26273 and  p = 0.1646
test <- subset(sets_cleaner, sets_cleaner$Theme == "Dreamzzz")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")

#Sonic the Hedgehog failed w/ mean = 56.24 and p = 0.3753
test <- subset(sets_cleaner, sets_cleaner$Theme == "Sonic the Hedgehog")
t.test(x = test$USD_MSRP, mu = mean(sets_cleaner$USD_MSRP), alternative = "greater")
    
#combine into data field so that we can graph
theme <- c("Star Wars","Technic","Education","Mindstorms","Advanced Models","Harry Potter","Factory","Architecture","Master Builder Academy","The Lord of the Rings","Ideas","Creator Expert","The Simpsons","Jurassic World","The LEGO Ninjago Movie","Icons","Art")
avg <- c(77.37085,103.387,160.7821,117.1327,183.5224,83.23399,123.8899,72.26624,80.75492,82.24213,125.5094,196.4867,244.9668,69.3625,92.06118,219.1679,137.0387)
pricey_themes <- data.frame(Theme = theme, Average = avg)

#create graph of expensive themes and their average price
library(ggplot2)
ggplot(pricey_themes, aes(x = pricey_themes$Theme, y = pricey_themes$Average)) + geom_boxplot() + labs(title = "Average Price of Various Expensive Themes", x = "Theme", y = "Average Price")





