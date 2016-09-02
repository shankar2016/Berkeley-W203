library(plotrix)

n=30
pi = 3.14159

set.seed(1212)

circleDataX <- runif(n, min=-1, max=1)
circleDataY <- runif(n, min=-1, max=1)
inOrOut <- logical(n)

circleDataFrame <- data.frame(circleDataX, circleDataY, inOrOut)

circleDataFrame$inOrOut <- ifelse(((circleDataFrame$circleDataX ^ 2) + (circleDataFrame$circleDataY ^ 2) <= 1), TRUE, FALSE)

circleIn <- subset(circleDataFrame, circleDataFrame$inOrOut == TRUE)
circleOut <- subset(circleDataFrame, circleDataFrame$inOrOut == FALSE)

plot.new()
frame()
#plot.window(xlim = 1.1 * 1 * c(-1, 1), ylim = 1.1 * 1 * c(-1, 1))
plot(circleDataX,circleDataY,asp=1,xlim=c(-1,1),ylim=c(-1,1))

plot(circleIn$circleDataX,circleIn$circleDataY,asp=1,xlim=c(-1,1),ylim=c(-1,1), pch = 10, col = "blue")

#points(circleIn$circleDataX, circleIn$circleDataY, pch = 10, col = "blue")
points(circleOut$circleDataX, circleOut$circleDataY, pch = 20, col = "red")

draw.circle(0,0,1,nv=1000,border=NULL,col=NA,lty=10,lwd=10)

rect(-1,-1,1,1)

areaOfSquare <- 2 * 2

computedPi <- areaOfSquare * (nrow(circleIn) / n)

test <- vector(mode="integer", 1000)

