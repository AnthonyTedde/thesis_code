# options
options("scipen" = 999)
options(digits = 7)
# Load libraries
library(ggplot2)
# clear workspace
rm(list = ls())
gc()
################################################################################
# Variables names
################################################################################
# p: Probability of Head
# q: Probability of Tail (1 - p)
# size: Number of periods for the Random Walk (start at 0)
# spread: Number of generated Random Walks
# X: Random variable which take value in {-1, 1}
#   -> 1  for each head with probability p
#   -> -1 for each tail with probability q
# M: symmetric random Walk -> construction based on X
# Mt: Random variable equiped with a filtration F(t)
# fi: Distribution of Mt
# l and k: both are time in the interval: [1, size + 1]
# 

################################################################################
# Create a 300 steps random walk
################################################################################
##
# Initialisation
##
p <- 1/2
q <- 1 - p
size <- 300
spread <- 3000
M <- 0
Mt <- fi <- data.frame(matrix(rep(0, (size + 1)^2), nrow = size + 1))
l <- 300
k <- 200
##
# Generation
##
X <- sample(x = c(-1, 1),
            size = size,
            replace = T,
            prob = c(p, q)
            )
for(i in 1:size) 
  M[i + 1] <- sum(X[1:i]) # i + 1 because M[0] from theory is indeed represented by M[1] in this code.
##
# Check by plot
##
png(filename = 'SymmetricRandomWalk.png')
 plot(M,
     type = 'l')
dev.off()

################################################################################
# Creation of a 300 steps random walk
################################################################################
##
# Construction of the theoretical Random Variable M based on the filtration F(t)
# See document "randomWalkDistributionAnalysis.jgp"
##
for(j in 1:(size + 1))
  for(i in 1:j)
    Mt[i,j] <- (j-i) + (1-i)
##
# Construction of the distribution of the theoretical random variable
##
for(j in 1:(size+1))
  for(i in 1:j)
    fi[i, j] <- choose((j-1), 
                       (j-i)) * p^(j-1)

##
# Graph of the Theoretical distribution
##

png(filename = 'SymmRandWalkTheoretiaclDistrib.png')
fromto <- 1:size + 1
to <- size + 1
plot(Mt[fromto, to],
     fi[fromto, to], 
     type = 'l', 
     xlim = c(-75, 75))
dev.off()

################################################################################
# Check the martingale property of symmetric random walk.
################################################################################
#
# With l > k, show that:
# E[Ml|F(k)] = Mk

##
# first: simple NOT CONDITIONAL Expectation
# E[M200] should equal zero because:
# M0 = 0 and Because Mt is a martingale thus:
# E[M200|f(0)] = E[M200] - 0
##
EM200 = sum(Mt[1:200, 200] * fi[1:200, 200]) # equal zero.

##
# Expectation of Mt_l at k
# denoted by: E[Mt_l|f(k)], with k < l
##
##
# Variables
##
from <- 2 # Departure of the Expectation
k <- 4 # To get the filtration point 
l <- 19 # Give the period to be expected
interval <- l-k
df <- data.frame(matrix(
  rep(0, (interval+1)^2),
  nrow = (interval+1)
))
##
# Partionated Symmetric Random Walk
##
df[1:(1+interval), 1:(1+interval)] <- Mt[from:(from + interval), k:l]
##
# Con 
##

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

























