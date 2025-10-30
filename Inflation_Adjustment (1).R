#using inflation data to equalize comparative buying power of USD data for each set
#first imported inflation data into excel and used a formula to translate the
#inflation data into an accumulation factor that would bring any monetary value 
#into the equivalent purchasing power to 2023 the year of our latest set in the 
#cleaned data
#formula:
#accumulation factor a(n) = (1+i/100)*(a(n-1)) , where
#i is the inflation rate expressed as a percentage
#a(n) is this year's accumulation factor and a(n-1)is the next year's accumulation
#factor and a(0) is the accumulation factor of 2023 which is 1
#n is the number of years away from 2023 i.e. n=1 -> 2022, n=2 ->2021, n=20->2003

#import inflation data
library(readr)
inflation <- read_csv("MAT 782/Project 1/Inflation adjustment/SeriesReport-20251028175929_9d5685.xlsx - BLS Data Series.csv")
View(inflation)

#then use the accumulation factor to accumulate the USD_MSRP
#create new dataframe for accumulation
sets_cleaner <- sets_clean
View(sets_cleaner)
#loop through dataframe to accumulate based on corresponding year
for(i in 1:nrow(sets_cleaner)){
  #set accumulation factor based on year
  a = inflation$...16[sets_cleaner$Year[i]-1978]
  #accumulate
  sets_cleaner$USD_MSRP[i] <- sets_cleaner$USD_MSRP[i]*a 
}

#repeat process for minifigures dataframe
sets_cleaner_minifigures <- sets_clean_minifigures
View(sets_cleaner_minifigures)
#loop through dataframe to accumulate based on corresponding year
for(i in 1:nrow(sets_cleaner_minifigures)){
  #set accumulation factor based on year
  a <- inflation$...16[sets_cleaner_minifigures$Year[i]-1978]
  #accumulate
  sets_cleaner_minifigures$USD_MSRP[i] <- sets_cleaner_minifigures$USD_MSRP[i]*a 
}

#recreate graphs for USD_MSRP
hist(sets_cleaner$USD_MSRP, xlab = "USD_MSRP", ylab = "Frequency", main = "Sets Released per Price")
