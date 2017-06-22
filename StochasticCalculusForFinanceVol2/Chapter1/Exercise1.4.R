# clear workspace
rm(list = ls())
gc()
################################################################
# Exercice 1.4
################################################################
 ################################################################################
# Variables names
################################################################################
# n: number of iteration of Yn (in Example 1.2.5)
# Y_n: according to example 1.2.5, it is the RV Y with a n equal to 20
# X : sum(Y/2^n)
##
# Initialisation
##
n <- 15

l <- rep(list(0:1), n)
# assign(Yn,
#        expand.grid(l))
Yn <- expand.grid(l)

seqn <- seq_len(n)
seq2n <- seq_len(2^n)

for(j in seqn)
  for(i in seq2n)
    Yn[i,j] <- Yn[i,j] / 2 ^ j
 
X <- rowSums(Yn)
X[order(X)]

####
# Check Probability measure
####
m <- 3000
k <- 10
(m/2^n) - (k/2^n)

length(X[X < (m/2^n) & X > (k/2^n)]) / length(X)

####
# Transform the random variable uniformly distributed to a standard normal distributed random variable
####
# First step: reverse the Cumulative Normal Distribution





















