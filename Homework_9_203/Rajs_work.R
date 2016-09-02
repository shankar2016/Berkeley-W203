library(fGarch)
library(psych)
library(ggplot2)
library(qdap)
library(stats)
library(car)
library(reshape2)
# Load Data
df = read.csv("../Homework_9_203/united_states_senate_2014.csv")

# Check Column Names
names(df)

# Summary Stats
summary(df)

# Review top top 10 records and basic size check on the table structure
head(df)
length(df)


## qq plot to check normality of variables.
qqnorm(df$Campaign.Money.Raised..millions.of...)
qqnorm(df$Campaign.Money.Spent..millions.of...)

## Histogram to check visualize distributions
hist(df$Campaign.Money.Raised..millions.of...)
hist(df$Campaign.Money.Spent..millions.of...)

#1. Is there a difference between the amount of money a senator raises and the amount spent?

#Calculate Mean of Money raised and Money Spent
mean(df$Campaign.Money.Raised..millions.of..., na.rm = TRUE)
mean(df$Campaign.Money.Spent..millions.of..., na.rm = TRUE)

# Null Hypothesis: There is no differece between means of Campaign money spent and Campaign Money Raised
# Alternative Hypothesis: There is differece between means of Campaign money spent and Campaign Money Raised

# Check Sample Size
nrow(df)

# type of variable
typeof(df$Campaign.Money.Spent..millions.of...)
typeof(df$Campaign.Money.Raised..millions.of...)

## Sample size is large enough to run a parametric test. We will run a paired t-test as there is pairing
## between money raised and money spent variable and it is for the same subject
senspend.t = t.test(df$Campaign.Money.Raised..millions.of..., df$Campaign.Money.Spent..millions.of..., paired=TRUE)
senspend.t

## Check for Effect Size
t = senspend.t$statistic[[1]]
dfreed = senspend.t$parameter[[1]]
r = sqrt(t^2/(t^2 + dfreed))
round(r,3)


#2. Do female Democratic senators raise more or less money than female Republican senators?

## Subset to Female Senators
df2 = subset(df[which(df$Gender=="Female" & df$Party != "NA"),])

## Top 10 records
head(df2)

## drop the party-type = "Independent"
df2$Party = droplevels(df2$Party)

## Total Sample size and Group level sample size
nrow(df2)
summary(df2$Party)

## Compute mean money raised by female senators grouped by party affiliation
by(df2$Campaign.Money.Raised..millions.of..., df2$Party, mean, na.rm=TRUE)


#Normality check
qqnorm(df2$Campaign.Money.Raised..millions.of...)
#shapiro.test(df2$Campaign.Money.Raised..millions.of...)

## Calculate Mean of Money Raised for Republican and Democrat Female Senator groups
by(df2$Campaign.Money.Raised..millions.of..., df2$Party, mean, na.rm=TRUE)


## Too small a sample, we cannot use Central limit theorem. We will run a non-parametric test
## Since this is different subjects, we will have to use a independent non-parametric test. So we will pick
## Wilcox Rank Sum Test

wilcox.test(df2$Campaign.Money.Raised..millions.of... ~ df2$Party)


#3 Do protestant Senators spend more or less money than non-protestant senators?

## Create a new variable protestant and non-protestant
df$protestant = factor(ifelse(df$Religion == "Protestant", "Protestant", "Non-Protestant"))

## Total Sample size, group level sample size
nrow(df)
summary(df$protestant)
## Sample size large enough to run parametric test

#df2 = subset(df[df$Religion == c("Female",])

## qqplot to check for normality
qqnorm(df[df$protestant == "Protestant",]$Campaign.Money.Spent..millions.of...)
qqnorm(df[df$protestant == "Non-Protestant",]$Campaign.Money.Spent..millions.of...)

#Compute Levene's test
leveneTest(df$Campaign.Money.Spent..millions.of...~ df$protestant, df)

##Homogeneity of Variance is not met, but that can be solved by running welch's t-test
##as well

## Calculate mean by protestant and non-protestant group.
by(df$Campaign.Money.Spent..millions.of..., df$protestant, mean, na.rm=TRUE)

## Run a conservative Non-parametric independent test. Wilcox Rank Sum
wilcox.test(df$Campaign.Money.Spent..millions.of... ~ df$protestant)

## Run a parametric independent t-test as normality is met by CLT,
## Homogeneity of variance can be corrected by Welch's t-test

test2= t.test(df$Campaign.Money.Spent..millions.of... ~ df$protestant, paired=FALSE)
test2

## Effect Size
t2 = test2$statistic[[1]]
dfreed2 = test2$parameter[[1]]
r2 = sqrt(t2^2/(t2^2 + dfreed2))
round(r2,3)