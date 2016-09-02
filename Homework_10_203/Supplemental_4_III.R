
# Natarajan Shankar
# W 203: Supplement Exercie 4
# July 20th

# Problem 1: BOOTSTRAPPING
#

# Play it safe and set a seed value to get repeatable results
set.seed(314156)

# Pull in needed libraries
library(ggplot2)

# FUNCTION: median_calculator()
# Input: Number of iterations, NBS
# Function that 

# Chose number of samples to be 1001. An odd number helps read the meadian vector easie
NBS <- 1001

# Set the confidence interval at 95%, split evenly across two tails
LOW_CI <- HIGH_CI <- 0.95/2

# Iteration processing
bootstrap_median <- function (iteration, parent_sample) {
  
  # Take a child sample from the parent sample, with replacement.
  # Child sample has the same length as the parent sample
  child_sample <- sample(parent_sample, size = length(parent_sample), replace = TRUE)
    
  # Calculate the median of the new sample
  child_median <- median(child_sample)
  
  # return the calculated mean
  return(child_median)
}


# Main bootstrap processing function
#   1. Takes a parent sample (from one of Random Normal, Chi Square, Binomial)
#   2. Takes the number of bootstrap iteraions (NBS) 
#   3. Iterates over each sample and extracts a median
#   4. Returns a collection of bootstrapped medians and CI interval
median_bootstrap <- function(parent_sample, NBS) {
  
  median_distribution <- sapply(1:NBS, bootstrap_median, parent_sample = parent_sample)
  
  # compute the 95% confidence interval
  low_CI <- quantile(median_distribution, LOW_CI)
  high_CI <- quantile(median_distribution, HIGH_CI)
  
  # Function returns the median distribution and confidence intervals
  return(list(median_distribution, c(low_CI, high_CI)))
}


# Test the median bootstrap using 3 kinds of data distributions
# 1. Random Normal
# 2. Poisson
# 3. Chi Square
# Plot results for each
test_code <- function() {
  # Random Normal Distribution
    parent_sample <- rnorm(n=100, mean=0, sd =1)
    # y <- dnorm(parent_sample)
    # plot(parent_sample,y)
    results <- median_bootstrap(parent_sample, NBS)
    browser()
    distribution_hist(results, "Random Normal Distribution")
    
  # Poisson Distribution
    parent_sample <- rpois(300, 4)
    # y <- dnorm(parent_sample)
    # plot(parent_sample,y)
    results <- median_bootstrap(parent_sample, NBS)
    distribution_hist(results, "Poisson Distribution")
    
  # Chi Square Distribution
    parent_sample <- rchisq(300, df=7)
    # y <- dnorm(parent_sample)
    # plot(parent_sample,y)
    results <- median_bootstrap(parent_sample, NBS)
    distribution_hist(results, "Chi Square Distribution")
  

  
}

# Use one common plot routine that can be used by the test code. 
# Customize the header strings so that each plat is distinguished by title
distribution_hist <- function (results, type = "") {
    # Plot the sampling distribution
    #hist(results[[1]], breaks = 30, main = "Median Distribution (Samples from Random Distribution)", xlab = "Median Value")
    resultsDF <- data.frame(results[[1]])
    myPlot <- ggplot(resultsDF, aes(results[[1]]))
    myPlot <- myPlot + geom_histogram(binwidth=0.02)
    myPlot <- myPlot + ggtitle(paste ("Median Distribution (Samples from ", type, ")")) + 
                labs(x="Median Value", y="Frequency")
    #plotName <- gsub(" ", "", paste("myPlot_", type, sep=""))
    #assign(plotName, myPlot)
    #plotName 
    myPlot
}
    
    


x <- seq(-1,20,by=.1)
generate_sample <- rchisq(300, df=7)
results <- sapply(1:1001, median_calculator, test_sample= generate_sample)

resultsDF <- data.frame(results)
myPlot <- ggplot(resultsDF, aes(results))
myPlot <- myPlot + geom_histogram(binwidth=0.1)
myPlot

y <- dchisq(x,df=7)
plot(x,y)


# Problem 2: OPTIMIZATION PROBLEM
#

# Function : isEmpty()
# Support routine to determine whether a vector is empty
# Used after cleaning up negative numbers fromm user input.
# If after cleanup of negative numbers, the vector is null, do not process further
isEmpty <- function(x) {
  return(length(x)==0)
}


# Function: objf()
# Scaffolding Cost function for Optim
# Take 3 numbers, x, r, and k. Raise r to the power of k and compare with X
# Return square of the compare

objf <- function(r, numbers, k) {
  
  cost = (numbers - (r^k)) ^2

  return(cost)
}

# Function: rootk()
# Function to find the root k of a positive number using numerical optimization
# Accept a vecor of numbers, numbers
# Return root k for all the numbers in the vector

rootk <- function(numbers, k, start = NULL) {
  
  if (!isEmpty(x <- numbers[numbers < 0])) {
    cat("CAUTION 0001 :  negative numbers in vector, being removed... \n\n\n")
    numbers <- numbers[numbers >= 0]
    
    # Stop is no numbers to process after negative number cleanup
    if(isEmpty(numbers))
        stop("Error 001: Input had negative numbers, cleaned up vector is empty!, Processing stopped")
  }
  
  # In case the user does not provide a starting approximation start at 1
  is.null(start)
    start = 1
   
  # User vector has positive numbers and has been sanitized
  # apply Optim() to all members in the vector. Return a vecor of results
  # Use method BGFS for optimized processing
  roots <- sapply(numbers, function(i) {optim(par = i, numbers = i, objf, k = k, method="BFGS")$par})
    result <- list(numbers, roots)
    names(result) <- c("Processed Numbers", paste ("Corresponding", k, "th Root for each processed number"))
    return(result)

}


rootk(8, 3)

rootk(625,4, start = NULL)

rootk(134217728,9)

rootk(c(25, -125, 96), 3)

rootk(c(25, -125, -96), 5)

rootk(c(-25, -125, -96), 3)



