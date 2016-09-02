


rm(primeList)

prime.sieve = function (n=1000) {
  
  primeList <- seq(2, n)
  for (i in seq(2,sqrt(n))) {
    for (j in seq(2, n/i )) {
      
      if (i*j > n) {
       break
      }
 
      if (is.element(i*j, primeList))
        primeList <- primeList[which(primeList != i*j)]

    }   
  }
  return(primeList)
}

prime.sieve(22)



result = replicate(n=100, expr=prime.sieve())
result2 = replicate(n=1000, expr=prime.sieve())

plot(density(result))
lines(density(result2), col='red')


results <- integer(1000)
totalPoints <- integer(1000)
iterations <- 0

while (iterations < 1000) {
  totalPoints[i] <- sample(100:1000, 1)
  results[i] <- variance.estimate(totalPoints[i])
  iterations <- iterations + 1
}
