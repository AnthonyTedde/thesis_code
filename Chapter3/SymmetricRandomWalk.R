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

# Theoretical distribution

png(filename = 'SymmRandWalkTheoretiaclDistrib.png')
plot(Mt[1:300, 300],fi[1:300, 300], type = 'l', xlim = c(-75, 75))
dev.off()

# Expectation
# E[M200] should equal zero because:
# M0 = 0 and Because Mt is a martingale thus:
# E[M200|f(0)] = E[M200] - 0
EM200 = sum(Mt[1:200, 200] * fi[1:200, 200]) # equal zero.

# Expectation of Mt_l at k
# denoted by: E[Mt_l|f(k)], with k < l
from <- 2
k <- 4
l <- 6
Mt[from:(from+l-k), k:l]
df <- data.frame(matrix(
  rep(0, (l-k+1)^2),
  nrow = (l-k+1)
))
for(i in from:(from+(l-k))) # ROWS
  for(j in k:l) # COLUMNS
    df[i - from + 1, j - k +1] <- Mt[i, j]
fi <- data.frame(matrix(rep(0, (l-k + 1)^2), nrow = l-k + 1))
for(j in 1:(l-k+1))
  for(i in 1:j)
    fi[i, j] <- choose((j-1), (i-1)) * p^(j-1)#((j-1)*p^(j-1))/(factorial(j-1)*factorial(1+i))
# Finally compute the expectation E[Mt_l|f(k)]:
sum(df[, l-k+1] * fi[, l-k+1]) #Yeah it is a fucking matringale


    

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

























