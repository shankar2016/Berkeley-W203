# Pull in all needed libraries
library("ggplot2")

# Create the base data for a Normal distribution
xNormal   <- seq(-5, 5,length=1000)
yNormal   <- dnorm(xNormal,mean=0, sd=1)

# Create  a dataframe for the Normal distribution with the created data to facilitate ggplot 
dfNormal <- data.frame(xNormal,yNormal)

# Create t-distributions for various levels of degrees of freedom
yDF10 <- dt(xNormal, 9) # Sample size is 10, df is 10 - 1, that is 9
yDF25 <- dt(xNormal, 24) # Sample size is 25, df is 25 - 1, that is 24
yDF199 <- dt(xNormal, 199) # Sample size is 200, df is 200 - 1, that is 199

# Create a data frame with all 3 distributions together
xAxis=c(xNormal, xNormal, xNormal, xNormal)
yAxis <- c(yNormal, yDF10, yDF25, yDF199)
tags <- rep(c("Normal", "tD with df=9", "tD with df=24", "tD with df=199"), each = 1000)
dfT <- data.frame(xAxis, yAxis, tags, each=1000)

# Plot just the base Normal Curve
compositePlot <- ggplot(dfNormal, aes(xNormal, yNormal))
compositePlot <- compositePlot + geom_line(size=1.5, col="Red")
 
# Now plot the 3 t Distributions
compositePlot <- compositePlot + geom_line(data = dfT, aes(xAxis, yAxis, col=factor(tags))) + geom_line(size=1.0) + 
  ggtitle("Normal distribution and \n t-distribution with df = 9, 24, 199") + # Set the title
  labs(x="X", y="Density") + # Set the axes
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) + 
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) +  # Set theme for text
  theme(legend.title=element_text(colour="Black", size=16, face="bold")) +  # Set the legend
  scale_colour_discrete(name = "Colors for Normal \nand T-Distribution") + # Set the legend
  #guides(col = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(limits = c(0.0, 0.5)) + 
  scale_x_continuous(limits = c(-5, 5)) +
  geom_vline(x=1.97)

# Plot and capture the 4 plots
compositePlot

# Zoom in on the tail
compositePlot + coord_cartesian(xlim = c(1.75,4.5),ylim= c(0,0.1)) + ggtitle("Tail Zoom: Normal distribution and \nt-distribution with df = 9, 24, 199")

# Zoom in on the peak
compositePlot + coord_cartesian(xlim = c(-1.0,1.0),ylim= c(0.3,0.4)) + ggtitle("Zoom at Mean: Normal distribution and \nt-distribution with df = 9, 24, 199")




# Naive use of cutoff line at z=1.96, which corresponds to 0.025% confidence interval for 2 tailed Normal Distribution


# For the t Distribution
# Interactive function to estimate alpha levels given a Z value
# and to estimate an error percentage
twoTailAlpha.t <- function() {
  cat("\n","\bEnter test cutoff Z value : ","\n")
  cutoffValue <- scan(n=1, what = numeric(0), quiet=T)
  
  cat("\n","\bEnter sample size for t-Distribution")
  sampleSize <- scan(n=1, what = numeric(0), quiet=T)
  
  # Compute and output the alpha value for the given Z value
  tDistributionAlpha <- (1 - pt(cutoffValue, sampleSize - 1))
  cat("\nComputed Real Alpha Value for t Distribution with df=", sampleSize -1, "is", tDistributionAlpha)
  
  # For the specific case of Z = 1.96, point out the magnification in Type 1 error area
  if (cutoffValue == 1.96 & abs(tDistributionAlpha) > 0.025) {
      cat(", \nthe Type I error zone is magnified because of the incorrect use of Z value")
      cat("\nEstimated maginfication in error =", 100* (tDistributionAlpha - 0.025)/0.025, "% \n")
  }
}

twoTailAlpha.t()

> qt(0.025, 200)
[1] -1.971896
> qt(0.025, 25)
[1] -2.059539
> qt(0.025, 10)
[1] -2.228139
> 

