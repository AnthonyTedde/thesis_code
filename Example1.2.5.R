#################################################################
# Example 1.2.5: Another random variable uniformly distributed.
################################################################
# W random variables included various value from 0 to one.
# All UNIFORMALY distributed.
W <- seq(from = 0,
         to = 1,
         by = .0001)
# Expected value of random variable W:
# n^-1 * sum(wi) or mean():
mean(W)
# using UNIFORM distribution.
punif(W)[length(W) / 2]
# 
# Define the probability measure p & q
p <- 1/2
q <- 1 - p
# number of exeriments: expe
expe <- 100000
# Sample size:
n <- 100
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







 














