# 
# Strong law of large number. An example of the theoretical concept of
# "Almost Surely Convergence"
# 

# Set the probability:
p <- 0.5

# Define the value taken by Yk
# Y represents all the possibility taken by the random variable for a period of
# k coin tosses.
k <- 20
Y <- expand.grid(
  rep(list(c(1,0)), k)
)

# H is also a random variable which denotes the number of head gains:
H <- rowSums(Y)

EH3 = 1/(2^k) * sum(H/k) 

# 
# Does it that means that the eq: lim n->inf (H/n) = 1.2 means that the expected
# value of the random variable H/n = 0.5 ?
#
