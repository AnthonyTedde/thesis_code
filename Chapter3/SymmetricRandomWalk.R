# Load libraries
library(ggplot2)
# clear workspace
rm(ls())
gc()
 ########################################
# Variables names
########################################
# Mt: Random Variable symmetric random Walk
########################################
# Create a 300 steps random walk
########################################
p <- 1/2
q <- 1 - p
size <- 300  # Because M[1] will in fact be M[0] = 0
spread <- 3000
X <- sample(x = c(-1, 1),
            replace = T,
            prob = c(p, q),
            size = size)
M <- 0
for(i in 1:size) 
  M[i + 1] <- sum(X[1:i]) # i + 1 because M[0] from theory is indeed represented by M[1] in this code.

##
# Check by plot
##
png(filename = 'SymmetricRandomWalk.png')

plot(M,
     type = 'l')
dev.off()

##
# Check the martingale property of symmetric random walk.
##
# With l>k
# E[Ml|Fk] = Mk
# 
# take a k and l between 1 and size.
l <- 300
k <- 200
M[k]

# Construction of the theoretical random variable M according to the filtration F(t)
Mt <- data.frame(matrix(rep(0, (size + 1)^2), nrow = size + 1))
for(i in 1:size)
  for(j in 0:(i - 1))
    Mt[j+1,i] <- (i - 1) - 2*j

# Construction of the distribution of the theoretical random variable
fi <- data.frame(matrix(rep(0, (size + 1)^2), nrow = size + 1))
for(j in 1:size)
  for(i in 1:j)
    fi[i, j] <- choose((j-1), (i-1)) * p^(j-1)#((j-1)*p^(j-1))/(factorial(j-1)*factorial(1+i))


########################################
# Create a 300 steps random walk
########################################
size <- 3000
spread <- 300000
Xs <- list()
for(i in 1:spread)
  Xs[[i]] <- sample(x = c(-1, 1),
                    replace = T,
                    prob = c(p, q),
                    size = size)

Wn <- vector()
for(i in 1:spread)
  Wn[i] <- sum(Xs[[i]])
png(filename = 'RandomWalkDistribution.png')

hist(Wn)
dev.off()

























