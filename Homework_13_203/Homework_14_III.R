# ## Homework 4, Bonus Question Part 1
# 
# For a random variable X, the mean is: 
#   $$m_x = E(\bar{X})$$
#   
#   The Variance is:
#   $$\sigma^2    = E[X - \bar{X}]^2$$
#   $$\sigma^2    = \bar{X^2} - (\bar{X})^2$$
#   
#   = E[X^2]  - (E[X])^2                 **Equation 1**
#   
#   
#   If 
# $$C_i$$ is points inside circle
# $$C_t$$ is total points plotted
# 
# If the probability p that a point lies inside the circle is 1 and if the probability that a point lies outside the circle is 0:
#   $$ \frac{\pi}{4}      = \frac{C_i}{C_t}$$
#   $$ E[X] = (\frac{\pi}{4}) * 1 + (1 - \frac{\pi}{4}) * 0 = (\frac{\pi}{4})$$
#   $$ E[X]^2 = (\frac{\pi}{4}) * (1^2) + (1 - \frac{\pi}{4}) * (0^2) = (\frac{\pi}{4})$$
#   
#   Substituting back in Equation 1:
#   E[X^2]  - (E[X])^2 = $(\frac{\pi}{4}) - ((\frac{\pi}{4})^2)$
#   Which then is 
# $(\frac{\pi}{4}) * (1 - (\frac{\pi}{4}))$                                 
#   \textcolor{red}{ ** Using pi value of 3.14159, Variance estimate is 0.168548 **}
# 
# 
# or is the same as
# $\sigma^2    =     (\frac{C_i}{C_t} * (1 - (\frac{C_i}{C_t}))$    
#                       
#                       The above is the variance estimate of $(\frac{\pi}{4})$
#                       
#                       Hence, the Variance estimate of $\pi$ is 
#                     $\sigma^2    =     (1/16) * (\frac{C_i}{C_t} * (1 - (\frac{C_i}{C_t}))$     \textcolor{red}{** Variance - Equation 2**}
#                                                  
                                                 
                                                 
                                                 
install.packages("MASS")
install.packages("lmtest")
install.packages("gvlma")
install.packages("sandwich")

library(MASS)
library(lmtest)
library(gvlma) # global validation of linear model assumptions)
library(boot)
library(sandwich)

# Set a seed for repeatable results
set.seed(123456)

# Select number of data points
N <- 100

# Creating independent variables
# first, the covariance matrix for our multi-variate normal
# This determines the correlation of our independent variables
sig <- matrix(c(2, .5, .25, .5, 1, 0, .25, 0, 1), nrow=3)

# Now the variables
predictorVar = as.data.frame(mvrnorm(n = N, mu = rep(1,3), Sigma = sig))

# Name the predictor variables
colnames(predictorVar) <- c("X1", "X2", "X3")

b0 = 1 # per class notes
b1 = 2 # per class notes
b2 = -5 # per class notes

# Create the epsilon function with heteroskedasticity include
h <- function(x) { 1.0 + 0.4*predictorVar[,2]}

epsilon = suppressWarnings(rnorm(N,0,h(x)))

#epsilon <- exp(predictorVar[,1]) * rnorm(N)/100
predictedVal = b0 + b1*predictorVar[,1] + b2 * predictorVar[,2] + epsilon
plot(predictedVal, epsilon)
abline(lsfit(predictedVal, epsilon), col = "red")
#abline(b0,b1,col=2)

model1 <- p <- lm(predictedVal ~ X1 + X2 , data = predictorVar)

# test for Heterskedsticity
bptest(model1)


bootReg <- function(formula, data, indices){
  d<- data[indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#bootResults <- boot(statistic = bootReg, formula =  predictedVal ~ predictorVar[,1] + predictorVar[,2], data = predictorVar, R = 2000)
bootResults <- boot(statistic = bootReg, formula =  predictedVal ~ X1 + X2, data = predictorVar, R = 2000)
summary(bootResults)
bootResults


## the following commands lead to the same tests:
summary(model1)
coeftest(model1)

## a different covariance matrix can be also used:
## either supplied as a function
#coeftest(model1, df = Inf, vcov = vcovHC)
coeftest(model1, vcov = vcovHC)
## or as a matrix
coeftest(model1, vcov = vcovHC(model1, type = "HC0"))


vcovHC(model1, type = "HC")
sandwich_se <- diag(vcovHC(model1, type = "HC"))^0.5
sandwich_se

coef(model1)-1.96*sandwich_se
#(Intercept)           x 
#-0.66980780  0.03544496 
coef(model1)+1.96*sandwich_se
#(Intercept)           x 
#0.4946667   2.3259412 




#Part 2

# Create the data first for tailiness = 0.333
y <- rbinom(n=100, p=0.333, size = 1)

LLy <- function(b,y) {
  return(sum(log(ifelse(y==1, b, 1-b))))
}
  
# starting value
b0 <- 0.5

# We are optimizing with optim(), so use fnscale = -1
# ignore the warning that telss you the default methood is not 
# the best method for the problem at hand

# If there is convergence, the output of optim$par should be close to 0.333

result <- suppressWarnings(optim(b0, fn = LLy, y = y, control = list(fnscale = -1)))


# Generate data
binomY <- ifelse(predictedVal < 0, 0, 1)

# Generate the overall data
dataDF <- cbind(binomY, predictedVal, predictorVar)

# Number of 0 and 1 rows
nY1 <- nrow(dataDF[dataDF$binomY==1,])
nY0 <- nrow(dataDF[dataDF$binomY==0,])


# What is the likelihood that y_i is 1
y_true <- nY1/(nY1 + nY0)


# What is the likelihood that y_i is 0
y_false <- nY0/(nY1 + nY0)


logit <- function(y, x, b)
{
  sum_loglh <- sum(y*log(1 + exp(-(x%*%b))) + (1-y)*log(1 + exp(x%*%b)))
  return(sum_loglh)
}

b=as.matrix(c(b0, b1, b2))
logit(as.matrix(binomY), as.matrix(predictorVar), as.matrix(b))
result2 <- optim(b,fn=logit,y=as.matrix(binomY),x=as.matrix(predictorVar), method='BFGS',hessian=TRUE)
result2$convergence
result2$par
solve(result2$hessian)
s.e <- sqrt(-diag(solve(result$hessian)))




# *****************
# EXPERIMENTAL CODE'
set.seed(194812)
n <- 100
x <- rnorm(n)
residual_sd <- exp(x)
y <- 2*x + residual_sd*rnorm(n)


