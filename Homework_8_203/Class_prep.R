# Details about the insurgency exercise. This is given in advance just in case you want to check it out on your own before coming to class. It will be done in small groups in breakout rooms. 
# 
# Insurgency Data Set Exercise (see insurgency.dta)
# 1) Each observation is an insurgency (with a state fighting an insurgent)
# -dur: duration of war in months 
# -wdl: win/draw/lose variable showing outcome from the point of view of the state
# -pol2: polity2 score for how democratic the state is. it ranges from -10 to +10 (10 means fully democratic)
# -occ: whether the state is an occupier fighting the insurgent on foreign land or is it fighting a domestic insurgency.
# 
# 2) Directions
# -use the "foreign" library and then "read.dta" to read the data 
# (e.g., insurgency = read.dta("blah\blah\Insurgency.dta")
#  
#  
#  3) Questions: 
#   
#   3a) What type of variable are each of the four variables mentioned above?
# 3b)   How can we study the following hypotheses:
#   * There is a relationship between occupation and war outcome .
# * Democracies fight shorter wars.
# 
# 3c)  What did you find? 
install.packages("foreign")
library(foreign)
install.packages("car")
library(car)
insurgency = read.dta("lyall2010.dta")
head(insurgency)



> typeof(insurgency$dur)
[1] "integer"
> typeof(insurgency$wdl)
[1] "integer"
> typeof(insurgency$pol2)
[1] "integer"
> typeof(insurgency$occ)
[1] "integer"
> cor(dur, wdl)
Error in is.data.frame(y) : object 'wdl' not found
> cor(insurgency$dur, insurgency$wdl)
[1] -0.0371993
> cor(insurgency$dur, insurgency$pol2)
[1] -0.05580994
> cor(insurgency$dur, insurgency$occ)
[1] -0.1620627
> cor(insurgency$wdl, insurgency$pol2)
[1] -0.002309073
> cor(insurgency$wdl, insurgency$occ)
[1] -0.1030881
> cor(insurgency$pol2, insurgency$occ)
[1] 0.2110921
> 
  
  
  > cor.test(insurgency$dur, insurgency$pol2)

Pearson's product-moment correlation

data:  insurgency$dur and insurgency$pol2
t = -0.942, df = 284, p-value = 0.347
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
-0.17068852  0.06056555
sample estimates:
cor 
-0.05580994 

> cor.test(insurgency$dur, insurgency$wdl)

Pearson's product-moment correlation

data:  insurgency$dur and insurgency$wdl
t = -0.6273, df = 284, p-value = 0.5309
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
  -0.15252469  0.07912554
sample estimates:
  cor 
-0.0371993 

> cor.test(insurgency$wdl, insurgency$occ)

Pearson's product-moment correlation

data:  insurgency$wdl and insurgency$occ
t = -1.7466, df = 284, p-value = 0.08179
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.21648313  0.01305144
sample estimates:
       cor 
-0.1030881 

> cor.test(insurgency$pol2, insurgency$occ)

	Pearson's product-moment correlation

data:  insurgency$pol2 and insurgency$occ
t = 3.6394, df = 284, p-value = 0.0003246
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
  0.09749568 0.31925909
sample estimates:
  cor 
0.2110921 

> cor.test(insurgency$dur, insurgency$occ)

Pearson's product-moment correlation

data:  insurgency$dur and insurgency$occ
t = -2.7677, df = 284, p-value = 0.006016
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.27291622 -0.04696193
sample estimates:
       cor 
-0.1620627 

> 


summary

