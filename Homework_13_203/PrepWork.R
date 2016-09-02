# W203
# Prep work for class, July 28, 2016
# Natarajan Shankar
# 

install.packages("lattice")
install.packages("lmtest")
install.packages("sandwich")
library(lattice)
library(car)
library(ggplot2)
library(lmtest)
library(sandwich)

# <cntrl> <shift> C
# Two things: 
#   
#   1-Working with data in a way that makes sense to humans:
#   
#   I recorded this screen capture to encourage you to use dplyr as your main data wrangling tool. 
# Take a look at the video (because the wall makes it awful, I just uploaded it on youtube).
# Here is a cheat sheet for it.
# There are many more resources that you can find online. There are even better videos on youtube, I'm sure. 
# But this is enough to get you started fast. 
# 
# 
# 2- Here is the data and instruction for this week's in class exercise.
# 
# I understand that you are busy with Lab #3, but do make sure you follow all of the async material and try your best to take a 
# stab at this before you come to class. Of course, we are going to work on this in class, but it is so much better if you spend 
# some time (even a few minutes) on it before you come to class.
# 
# Best,
# Ali
# 
# --------------------------
#   Using the Global Happiness Project Dataset (attached), you will examine the dependent variable "Do.you.love.and.appreciate.yourself."  
# 
# Look for an interesting independent variable that you think will have a linear relationship with the dependent variable. 
# (This is something that we are doing because this is an exercise; not something you'd do in the real world).
# Pick an independent variable that is either dichotomous or metric (or, perhaps a scale that you can justifiably treat as interval).
# For your own sanity, use "name" to change the name of columns you want to work with. You can do things like the following
# DFnames = names(DF)
# DFnames[ c(1, 7, 9) ]   = c('var1' , 'var7', 'var9')
# names(DF) = DFnames
# 
# 
# (a) Based on the type of independent variable that you choose, conduct either a correlation or a t-test on your two variables. 
# What can you conclude?
# 
# (b) Produce a scatterplot to examine whether your variables have a linear relationship. Given the type of independent and 
# dependent variables in your analysis, you may want to consider the limitations of your scatterplot.
# 
# Because our dependent variable only has a few levels of measurement, you will want to add some jitter to your scatterplot 
# (why?).  Add the following argument to your scatterplot function from the car package:jitter=list(x=1, y=1). 
# Alternatively, use geom_jitter if you want to use ggplot.
#  
# (c) Conduct a linear regression and interpret your findings
# 
# (d) Bonus: Execute (a) through (c) for any other IVs of interest.


# Read data file
Happiness <- read.csv("Happiness.csv", header = TRUE)

# Check out the data
head(Happiness)

# 
# Look at column names
names(Happiness)
# > names(Happiness)
# [1] "What.city.do.you.live.in."                                                      
# [2] "What.state.or.province.do.you.live.in..if.applicable."                          
# [3] "What.country.do.you.live.in."                                                   
# [4] "What.is.your.age."                                                              
# [5] "How.often.do.you.try.to.make.other.people.happy."                               
# [6] "Are.your.surroundings.physically.safe."                                         
# [7] "Do.the.people.you.re.close.to..family.friends..love.and.support.you."           
# [8] "How.often.do.you.listen.to.music.you.enjoy."                                    
# [9] "How.often.do.you.use.technology..games..social.media..etc...for.entertainment.."
# [10] "How.many.hours.of.uninterrupted.sleep.do.you.get.each.night."                   
# [11] "How.often.are.you.outdoors."                                                    
# [12] "Do.you.feel.respected.by.other.people."                                         
# [13] "How.much.exercise.do.you.get.weekly."                                           
# [14] "How.often.do.you.smile.every.day."                                              
# [15] "Do.you.love.and.appreciate.yourself."  <--- DEPENDENT VARIABLE                                           
# [16] "How.often.do.you.feel.happy.with.what.you.do.every.day..e.g..work..school.."    
# [17] "How.often.do.you.argue.with.people.who.are.close.to.you."                       

# Experiment to determine linear relationships
Scatterplot(Happiness$What.country.do.you.live.in. ~ Happiness$Do.you.love.and.appreciate.yourself., Happiness)


# Case I
model_1

ggplot(Happiness, aes(x=((Happiness$How.often.do.you.smile.every.day.)), y=((Happiness$Do.you.love.and.appreciate.yourself.)))) + 
  geom_point(shape=1) + geom_smooth(method=lm) + geom_jitter(width = 0.1, height=0.1) + geom_abline(intercept = 1.4905, slope = 0.5684, colour = "red")
pearson_result_1 <- cor.test(Happiness$Do.you.love.and.appreciate.yourself., Happiness$How.often.do.you.smile.every.day., method="pearson", conf.level=0.95, na.rm=TRUE)
pearson_result_1

outlierTest(model_1)
dwt(model_1)
bptest(model_1)
coeftest(model_1, vcov=vcovHC)

# Case II
model_2 <- lm(Happiness$Do.you.love.and.appreciate.yourself. ~ How.often.do.you.argue.with.people.who.are.close.to.you., Happiness)
model_2
ggplot(Happiness, aes(x=((Happiness$How.often.do.you.argue.with.people.who.are.close.to.you.)), y=((Happiness$Do.you.love.and.appreciate.yourself.)))) + 
  geom_point(shape=1) + geom_smooth(method=lm) + geom_jitter(width = 0.1, height=0.1)
pearson_result_2 <- cor.test(Happiness$Do.you.love.and.appreciate.yourself. , Happiness$How.often.do.you.argue.with.people.who.are.close.to.you., method="pearson", conf.level=0.95, na.rm=TRUE)
pearson_result_2
outlierTest(model_2)
dwt(model_2)
bptest(model_2)
coeftest(model_2, vcov=vcovHC)

# Case III
model_3 <- lm(Happiness$Do.you.love.and.appreciate.yourself. ~ What.is.your.age., Happiness)
model_3
ggplot(Happiness, aes(x=((Happiness$What.is.your.age.)), y=((Happiness$Do.you.love.and.appreciate.yourself.)))) + 
  geom_point(shape=1) + geom_smooth(method=lm) + geom_jitter(width = 0.1, height=0.1)
pearson_result_3 <- cor.test(Happiness$Do.you.love.and.appreciate.yourself. , Happiness$What.is.your.age., method="pearson", conf.level=0.95, na.rm=TRUE)
pearson_result_3
outlierTest(model_3)
dwt(model_3)
bptest(model_3)
coeftest(model_3, vcov=vcovHC)


# Case IV
model_4 <- lm(Happiness$Do.you.love.and.appreciate.yourself. ~ How.often.do.you.use.technology..games..social.media..etc...for.entertainment.., Happiness)
model_4
ggplot(Happiness, aes(x=((Happiness$How.often.do.you.use.technology..games..social.media..etc...for.entertainment..)), y=((Happiness$Do.you.love.and.appreciate.yourself.)))) + 
  geom_point(shape=1) + geom_smooth(method=lm) + geom_jitter(width = 0.1, height=0.1)
pearson_result_4 <- cor.test(Happiness$Do.you.love.and.appreciate.yourself. , Happiness$How.often.do.you.use.technology..games..social.media..etc...for.entertainment.., method="pearson", conf.level=0.95, na.rm=TRUE)
pearson_result_4
outlierTest(model_4)
dwt(model_4)
bptest(model_4)
coeftest(model_4, vcov=vcovHC)

# Case V
model_5 <- lm(Happiness$Do.you.love.and.appreciate.yourself. ~ as.numeric(Happiness$How.many.hours.of.uninterrupted.sleep.do.you.get.each.night.), Happiness)
model_5
ggplot(Happiness, aes(x=(as.numeric(Happiness$How.many.hours.of.uninterrupted.sleep.do.you.get.each.night.)), y=((Happiness$Do.you.love.and.appreciate.yourself.)))) +  geom_point(shape=1) + geom_smooth(method=lm) + geom_jitter(width = 0.1, height=0.1) + geom_abline(intercept = 1.4905, slope = 0.5684, colour = "red")
pearson_result_5 <- cor.test(Happiness$Do.you.love.and.appreciate.yourself., as.numeric(Happiness$How.many.hours.of.uninterrupted.sleep.do.you.get.each.night.), method="pearson", conf.level=0.95, na.rm=TRUE)
pearson_result_5
outlierTest(model_5)
dwt(model_5)
bptest(model_5)
coeftest(model_5, vcov=vcovHC)

#ggplot(Happiness, aes(x=Happiness$How.often.do.you.smile.every.day., y=Happiness$Do.you.love.and.appreciate.yourself.)) + geom_point(shape=1)   

#ggplot(Happiness, aes(x=Happiness$How.often.do.you.smile.every.day., y=Happiness$Do.you.love.and.appreciate.yourself.)) + 
#geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)

#ggplot(Happiness, aes(x=Happiness$How.often.do.you.smile.every.day., y=Happiness$Do.you.love.and.appreciate.yourself.)) + 
#    geom_point(shape=1) + geom_smooth()
