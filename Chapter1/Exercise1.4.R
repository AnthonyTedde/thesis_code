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
n <- 3
Yn <- paste0('Y', n)

l <- rep(list(0:1), n)
assign(Yn,
       expand.grid(l))

for(j in seq_len(n))
  for(i in seq_len(2^n))
    Y3[i,j] <- Y3[i,j] / 2 ^ j

X <- rowSums(get(Yn))
X[order(X)]





















