# 1. Construct the binomial stock price model
# The following function takes two arguments: 
#     (i)   Initial stock price (s)
#     (ii)  Number of period (integer. n)
binomialAssetPrice <- function(s = 4, n = 3){
  # matrix (n+1) * (n+1) due to S0
  cpt <- function(i, j){
    ifelse(i<=j, s * 2 ^ j / (4 ^ i), NA)
  }
  outer(0:n, 0:n, cpt )
}
S <- binomialAssetPrice(4, 3)