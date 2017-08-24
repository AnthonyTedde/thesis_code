#################################################################
# Example 1.2.5: Another random variable uniformly distributed.
################################################################
library(ggplot2)
# W random variables included various value from 0 to one.
# All UNIFORMALY distributed.
W <- seq(from = 0,
         to = 1,
         by = .0001)
# 
# Define the probability measure p & q
p <- 1/2
q <- 1 - p
# number of exeriments: expe
expe <- 100000
# Sample size:
n <- 100

##
# Construction of the theoretical distribution of X
#
# Not consider the infinite sample space but instead a rather large one
#
#   * [n]: It gives the number of columns.
#   * [2^n]: Gives the number of rows.
##
n <- 20
Y <- expand.grid(
  rep(
    list(c(1,0)), 
    n)
)
Y <- t(apply(Y, MARGIN = 1, FUN = function(x) x/2^(1:n)))
X <- sort(rowSums(Y), decreasing = T)

distrib_X <- data.frame('X' = X, 
                        'P' = 1/length(X),
                        'F' = seq(1/length(X), 1, by = 1/length(X)))

ggplot(distrib_X, aes(X, F)) +
  geom_point()
# Find the expected value:
prob_X <- 1/length(X)
sum(X * prob_X)

#
# mu[k/2^n, m/2^n] = m/2^n - k/2^n
# -> looks like the Lebesgue measure over interval [0,1]
#
# For instance, take:
#   * [m]: Numerator of the upper value of the closed interval
#   * [k]: Numerator of the lower value of the closed interval
#   * [n]: Same n as which one used to create the theoretical distribution
#   * [mu]: Value of the Probability the one have a value inside the interval.
#
#   -> The value obtained will be futher tested over sample 
#   -> The interval could be seen as quantile of the uniform probability measure
#       (to be tested)
#
k = .25 * 2^n
m = .75 * 2^n
# -> which will give results for percentile .25 up to .75

##
# Next: Convergence of Integrals.
##

# 
# Y should be constructed according to some random experiment.
Y = list()
for(i in 1:expe){
  Y[[i]] <- rbinom(n = n,
                   size = 1,
                   prob = p)
}
# According to Y, construction of X:
# X should be a random variable construct upon a vector
# Transformation of Y
YPrime <- Y
for(i in 1:expe)
  for(j in 1:n)
    YPrime[[i]][j] <- Y[[i]][j] / 2^j
    
X <- vector()
for(i in 1:expe)
  X[i] <- sum(YPrime[[i]])

# Probability that the value of X fall between 4/3000 and 5/3000:
ProbX <- 1 / 2 ^ n

####################################################
#  Check of the theory
 ####################################################

# Probability that X is in the interval:
# [0, 3e29/2^n]:
probI <- (3e29 - 0)/2^n
# Check:
length(X[X < 3e29/2^n]) / length(X)


# Probability that X is in the interval:
# [3e28/2^n, 3e29/2^n]:
probI <- (3e29 - 3e28)/2^n
# Check:
length(X[X > 3e28/2^n & X < 3e29/2^n]) / length(X)







 














