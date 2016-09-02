install.packages("QuantPsyc")
library(QuantPsyc)

library(boot) 

data <- read.delim("Album Sales 2.dat", header = TRUE, sep="\t") 

album2 <- data

bootReg <- function(formula, data, indices){
  #browser()

       d<- data[indices,]
       fit <- lm(formula, data = d)
       return(coef(fit))
   }

bootResults <- boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = album2, R = 2000)
summary(bootResults)
bootResults

bootResults2 <- lm(formula = sales ~ adverts + airplay + attract, data = album2)
summary(bootResults2)


lm.beta(bootResults2)