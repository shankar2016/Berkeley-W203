#load the GSS1994_daughters dataset.
load("GSS_daughters.rdata")

desc = function(var, df) {
  attr(df,"var.labels")[match(var, colnames(df))]
}

cbind(names = colnames(GSS), labels = attr(GSS, "var.labels"))


levels(GSS$kdsex1)

# remove lines in kdsex1 that have "iap", "dk", "na"
GSS_data <- GSS[(GSS$kdsex1 != "iap" & GSS$kdsex1 != "dk" & GSS$kdsex1 != "na"), ]

# Remove "iap" frpm GSS_data$race
GSS_data <- GSS_data[(GSS_data$race != "iap"), ]
GSS_data$race <-  droplevels(GSS_data$race)
GSS_data$race_b <- ifelse(GSS_data$race == "black", 1, 0)
head(GSS_data)

# Remove NAs from "educ" column
GSS_data<- GSS_data[!is.na(GSS_data$educ),]
nrow(GSS_data)

GSS_data$kdsex1 = droplevels(GSS_data$kdsex1)


# Remove "na", "dk" in partyid
class(GSS_data$partyid)
levels(GSS_data$partyid)
GSS_data <- GSS_data[(GSS_data$partyid != "dk" & GSS_data$partyid != "na"), ]
GSS_data$partyid = droplevels(GSS_data$partyid)
class(GSS_data$partyid)
levels(GSS_data$partyid)

# Converty to numeric, check number of rows
GSS_data$partyid <- as.numeric(GSS_data$partyid)
class(GSS_data$partyid)
summary(GSS_data$partyid)
nrow(GSS_data)


# Remove NAs
GSS_data<- GSS_data[!is.na(GSS_data$partyid),]
nrow(GSS_data)

# Recode “sex of the first child” (kdsex1) into a dummy variable (daughter1) 
# where female is coded as 1 or ‘true’.  Examine partyid and turn it into a 
# numeric variable (tabulate it to see what you need to do and be careful; 
# as.numeric should be helpful here.). This is your dependent variable.
# Recode kdsex1 as female = 1, other = 0
GSS_data$daughter1 <- ifelse(GSS_data$kdsex1 == "female", 1, 0)

# Conduct linear regression with daughter1 as independent variable
result1 <- lm(formula = partyid ~ daughter1, data = GSS_data)

#Call:
#  lm(formula = partyid ~ daughter1, data = GSS_data)
#
#Coefficients:
#  (Intercept)    daughter1  
#3.7423       0.2863  


# Conduct linear regression with daughter1 as independent variable and one or more additional variables
# Choose educ

result2 <- lm(formula = partyid ~ daughter1 + educ, data = GSS_data)
#Call:
#  lm(formula = partyid ~ daughter1 + educ, data = GSS_data)
#
#Coefficients:
#  (Intercept)    daughter1         educ  
#2.63749      0.29896      0.08488  


# Look at the data
summary(result1)
summary(result2)

#
class(GSS_data$race)
levels(GSS$race)

result3 <- lm(formula = partyid ~ daughter1 + educ + race_b, data = GSS_data)

# Look at the data
summary(result1)
summary(result2)
summary(result3)

result1.residuals
result2.residuals
result3.residuals

selected.vars = c('repub.partyid','daughter1','educ')
GSSsub = subset(GSS, select = selected.vars)
GSSsub = subset(GSSsub, subset = complete.cases(GSSsub))

m1<- lm(repub.partyid~daughter1     , data=GSSsub) 
m2<- lm(repub.partyid~daughter1+educ, data=GSSsub) 
anova(m1,m2)

install.packages("lmtest")
library(lmtest)
