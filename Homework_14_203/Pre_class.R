install.packages("car")
install.packages("mlogit")
install.packages("PASWR")
library(car)
library(mlogit)
library(PASWR)
? titanic3
summary(titanic3)
library(ggplot)
library(dplyr)

# You will run a logistic regression to predict the probability of survival 
# (survived).  Begin with one predictor variable, parch, representing the 
# number of parents and children that each individual had on the boat.
# 
# Create a plot of the actual survival percentages by parch. Comment on 
# whether the logistic model seems appropriate---hint: is it something 
# that has a logistic shape (flattened-s shaped) or can be part of that 
# shape, or not?
# 
# Run the logistic model and comment on both the statistical and 
# practical significance of your predictor. Add other independent 
# variables that you think make sense.
# 
# Use "predict" to predict probabilities for changes in one of 
# your independent variables. 
# 
# for example, if you have estimated "model1," you can predict 
# probabilities out of your model by doing this:
#   
#   mydata  = data.frame( parch = 0:10 )
# mydata$predicted.probability = predict( model1, newdata = mydata, type='response' )
# 
# 
# Continue exploring the dataset and find something interesting to share with the class! 
# You may want to include parch^2 as well. Remember that you need to use I(parch^2) on 
# the right hand side to prevent glm from trying to interpret your parch^2  in some other way. 
# 
# 
# - Use the anova command to compare the two models.
# - Plot predicted probabilities against the actual observation of survival rate.


DT <- titanic3

names(DT)

SR = DT %>% group_by(parch) %>%
    summarize(survival.rate = mean(survived, na.rm=TRUE), count=n())
SR

ggplot(SR, aes(parch, survival.rate, size=count)) + 
  geom_line(size=0.7) +
  geom_point(color='orange')
model_titanic1 <- glm(survived ~ parch, data = titanic3, family = binomial())
summary(model_titanic1)


model_titanic2 <- glm(survived ~ parch + I(parch^2), data = titanic3, family = binomial())
summary(model_titanic2)


