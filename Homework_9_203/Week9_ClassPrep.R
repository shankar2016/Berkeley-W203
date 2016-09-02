# Natarajan Shankar
# W203
# Week 9, pre-class work

install.packages("ggplot2")
install.packages("pastecs")
install.packages("WRS")
install.packages("rpsychi")

library(pastecs)
library(rpsychi)
library(ggplot2)

# read the data
senateData <- read.csv("united_states_senate_2014.csv", header=T)

# Create a smaller data frame with just the funding data
fundingData <- data.frame(senateData$Campaign.Money.Raised..millions.of..., senateData$Campaign.Money.Spent..millions.of...)

# Dependent t test
# Null Hypothesis : Population difference in Amount of money raised and Amount of money spent is 0
# Alternate Hypothesis : Population difference in Amount of money raised and Amount of money spent is NOT 0


# Look at the Normality of the data 
qqnorm(fundingData$senateData.Campaign.Money.Raised..millions.of..., main = "Money raised by Senators")
qqnorm(fundingData$senateData.Campaign.Money.Spent..millions.of..., main = "Money spent by Senators")

# Run the Dependent t-test, we are working with the same test group
dep.t.test<- t.test(fundingData$senateData.Campaign.Money.Raised..millions.of..., fundingData$senateData.Campaign.Money.Spent..millions.of..., paired=TRUE)
dep.t.test

#Paired t-test

#data:  fundingData$senateData.Campaign.Money.Raised..millions.of... and fundingData$senateData.Campaign.Money.Spent..millions.of...
#t = 5.9944, df = 99, p-value = 3.329e-08
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.9486232 1.8873768
#sample estimates:
#  mean of the differences 
#1.418 

# t is positive. This means that the first conditoin (Raised) had a larger mean than the second (Spent)
# Confidence interval does not include 0 : True value of the mean difference is unlikely to be Zero
# p value is low thus indicating the result is statistically significant

>
  
  
  
  
  
  
  
  
  
  
# Female Fundraising
  
# NULL Hypothesis : No differenec in fundraising between Female Democrats and Female Republicans
  
  
# get the date on the female senators
femaleFundraising <- senateData[senateData$Gender == "Female",]  
femaleFundraising
#Senator.Names Gender          State      Party     Religion Campaign.Money.Raised..millions.of... Campaign.Money.Spent..millions.of...
#2        Kelly Ayotte Female  New Hampshire Republican     Catholic                                   7.3                                  6.0
#3       Tammy Baldwin Female      Wisconsin   Democrat Unaffiliated                                  15.3                                 15.9
#11      Barbara Boxer Female     California   Democrat       Jewish                                  13.8                                  7.1
#14     Maria Cantwell Female     Washington   Democrat     Catholic                                  11.7                                 10.7
#22      Susan Collins Female          Maine Republican     Catholic                                   3.6                                  0.9
#31   Dianne Feinstein Female     California   Democrat       Jewish                                   9.7                                 11.4
#32        Deb Fischer Female       Nebraska Republican   Protestant                                   5.6                                  5.6
#35 Kirsten Gillibrand Female       New York   Democrat     Catholic                                  29.7                                 28.1
#38          Kay Hagan Female North Carolina   Democrat   Protestant                                   9.9                                  3.1
#42     Heidi Heitkamp Female   North Dakota   Democrat     Catholic                                   6.2                                  6.0
#44       Mazie Hirono Female         Hawaii   Democrat     Buddhism                                   5.9                                  6.0
#54      Amy Klobuchar Female      Minnesota   Democrat   Protestant                                   9.6                                  8.4
#55      Mary Landrieu Female      Louisiana   Democrat     Catholic                                   9.5                                  3.1
#62   Claire McCaskill Female       Missouri   Democrat     Catholic                                  20.9                                 20.7
#66   Barbara Mikulski Female       Maryland   Democrat     Catholic                                   4.8                                  4.7
#68     Lisa Murkowski Female         Alaska Republican     Catholic                                   5.2                                  4.9
#70       Patty Murray Female     Washington   Democrat     Catholic                                  14.5                                 16.4
#86     Jeanne Shaheen Female  New Hampshire   Democrat   Protestant                                   5.8                                  2.5
#88    Debbie Stabenow Female       Michigan   Democrat   Protestant                                  13.8                                 13.2
#97   Elizabeth Warren Female  Massachusetts   Democrat   Protestant                                  44.2                                 43.4
#>

#Isolate the data for female Senators
femaleDemocratmoneyRaised <- femaleFundraising[femaleFundraising$Party == "Democrat", ]$Campaign.Money.Raised..millions.of...
femaleRepublicanmoneyRaised <- femaleFundraising[femaleFundraising$Party == "Republican", ]$Campaign.Money.Raised..millions.of...


# Check the normality of the data
qqnorm(femaleDemocratmoneyRaised)
qqnorm(femaleRepublicanmoneyRaised)

# run the independent t-test
t.test(Campaign.Money.Raised..millions.of... ~ Party, data = femaleFundraising, paired = FALSE)

#Welch Two Sample t-test
#
#data:  Campaign.Money.Raised..millions.of... by Party
#t = 3.2476, df = 17.102, p-value = 0.004708
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  3.035148 14.277352
#sample estimates:
#  mean in group Democrat mean in group Republican 
#14.08125                  5.42500 



# Data does not appear to be Normal distribution
# Let's run the wilcox.test
wilcox.test(Campaign.Money.Raised..millions.of... ~ Party, data = femaleFundraising, paired = FALSE)


#Wilcoxon rank sum test with continuity correction

#data:  Campaign.Money.Raised..millions.of... by Party
#W = 58, p-value = 0.01593
#alternative hypothesis: true location shift is not equal to 0

#Warning message:
#  In wilcox.test.default(x = c(15.3, 13.8, 11.7, 9.7, 29.7, 9.9, 6.2,  :
#                                 cannot compute exact p-value with ties
#                               > 











#Part 3: Do protestant Senators spend more or less money than non-protestant senators?

# NULL Hypothesis: Protestant senators spend the same amount as non-protestant ministers

#Isolate the data for Protestent and non-Protestant Senators
spentMoneyProtestantSenators <- senateData[senateData$Religion == "Protestant", ]
spentMoneynonProtestantSenators <- senateData[senateData$Religion != "Protestant", ]

# Change all Non Protestents to "Non Protestant"
spentMoneynonProtestantSenators$Religion <- "Not Protestant"


# Cocatenate the two dataframes
newDF <- rbind(spentMoneyProtestantSenators, spentMoneynonProtestantSenators)

# run the independent t-test
t.test(Campaign.Money.Spent..millions.of... ~ Religion, data = newDF, paired = FALSE)


#Welch Two Sample t-test
#
#data:  Campaign.Money.Spent..millions.of... by Religion
#t = -1.2856, df = 97.177, p-value = 0.2016
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -4.857697  1.038329
#sample estimates:
#  mean in group Protestant mean in group Not Protestant 
#7.253061                     9.162745 
 

wilcox.test(Campaign.Money.Spent..millions.of... ~ Religion, data = newDF, paired = FALSE)


#Wilcoxon rank sum test with continuity correction

#data:  Campaign.Money.Spent..millions.of... by Religion
#W = 1023, p-value = 0.1191
# alternative hypothesis: true location shift is not equal to 0

#> 
