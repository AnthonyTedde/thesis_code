######################################################################
#
# Chapter 3 > 3.2 Scaled symmetric random walk
#
# TODO: section: Create an array of (3000) steps sample random walk
# Work to find a solution for functionnal way of doing the stuff
#
######################################################################

#
# Library
#
library(ggplot2)

#
# Probability measure:
#
# For the purpose of this theory, first are define the probabilities p and q such as it has the same occurence (one half)
#
p <- 1/2
q <- 1 - p

#
# Some graphics will be created inside this folder:
# code\Chapter3\figure
#
# To access the content of this folder, the variable figure could be used for:
#
figure <- paste(getwd(), 'Chapter3', 'figure', sep = '/')

#
# Definition of the symmetric random walk
#
# The symmetric random walk is first defined by some samples (which makes an array of sampled symmetric random walk)
# Variables:
#   * [k]: The size of the sample is denoted by the variable k -> (see theory $M_k$ denote the Symmetric Random Walk variable)
#   * [asize]: length of the array containing the Symmetric Random Walks
#
# Next it will be defined by its distributions.
#
# Both will be use full for finding expectation and variance theoterically or empirically
#
k <- 3000
asize <- 75000

###################################
# Empirical Symmetric Random Walk #
###################################
#
# Construction on the Empirical Random variable X
X <- sample(x = c(-1, 1),
            size = k,
            replace = T,
            prob = c(p, q)
)
# 
# Construction of the Empirical Random Walk
#
Mk <- c(0, 
         sapply(seq_along(X), function(x){sum(X[1:x])}))

##
# Check by plot
##
file <- paste(figure, 'EmpiricalSymmetricRandomWalkSample.pdf', sep = '/')
pdf(file = file)
plot(Mk,
     type = 'l')
dev.off()

#######################################################
# Create an array of [3000 steps] sample random walks #
#######################################################
#
#   * [Mkr]: Array of [asize] number of Symmetric Random Walk
#
Mkr <- matrix(data = sample(x = c(-1, 1),
                            replace = T,
                            prob = c(p, q),
                            size = k*asize),
              nrow = asize,
              ncol = k
)
 
#
# Calculate the empirical distribution
# First by computing the Expectation
#
#   *[Mki]: Array containing the final step of the random work at time F(T)
#           Only all the finals steps are keeping inside the new array
#           because the expectation computed is the following:
#           E[M_T|F(0)] -> The expectation of the value at time T computed from
#                           t = 0
#
#   *[Empir]:  Empirical expectation of Symmetric Random Walk. 
#             [Mki] contains a sample of symmetric Random Value for time = T
#             To compute the expectation I only use the function *mean* on [Mki]
#               According to the martingale properties of Symmetric Random Walk
#             E[M[T] | 0] = E[M[T]] = M[0] = 0
#
#
#   rowSums is used to compute the accumulated value of the random walk
#   from t=0 to t=T
Mki <- rowSums(Mkr)
Empir <- mean(Mki)

#
# Plot the distribution
#
#   * [Mkd] : Matrix that contains:
#     * [RandomValue] : Random variable in which are stored the accumulated values of 
#                       the Tth step of  all the Symmetric Random Walks
#
#     * [Frequency] : The frequency of each value before aggregation. Indeed
#                     before aggregate the values, all have the same occurence.
#
#   * [MkHistogramFormated] : Aggregate version of Mkd 
#                             (group by is made from [RandomValue])
#
Mkd <- cbind(RandomValue = Mki, 
             Frequency = 1/length(Mki))
#
# The following variable is the aggregation of the Frequency by RandomValue.
# It could be used with other plot engine that ggplot with geom_histogram
# where the aggregation is automated.
MkHistogramFormated <- aggregate(Frequency ~ RandomValue, Mkd, sum)

file <- paste(figure, 'EmpiricalDistribution.pdf', sep = '/')

pdf(file = file)
ggplot(as.data.frame(Mkd), aes(RandomValue)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 binwidth = 10) +
  scale_y_continuous(labels = scales::percent)
dev.off()
  
############################ 
# Theoretical distribution #
############################ 
#
#  *[Mt]: Stands for Theoretically Symm. Random Walk
# 
# The array (data.frame) used to store all the value that the variable Mt could take according to the path has a dimension dim_x * dim_y
#
# IMPORTANT NOTE: Due to precision issue, the variable k (which denotes the number of step
#       into the brownian motion) must be fixed at 1000 maximum. The issue occurs when
#       using the *choose* function in order to compute the distribution values.
#
k <- 1000
dim_x <- dim_y <- 1:(k + 1)
Mt <- outer(dim_x, 
           dim_y, 
           FUN=function(r,c){ifelse(c>=r, (c-r) - (r-1), NA_integer_)}) 

#
# Mt contains all the values the random variables could take
# it remains to derive all the probabilities associated to that values:
#
#   * [fi]: Dataframe which has same dimension than Mt and which gives the probability associated.
#
fi <- outer(dim_x,
           dim_y,
           FUN = function(i, j){choose((j-1), (j-i)) * p^(j-1)})
# CHECK: A rapid check to see if the variable [fi] contains probability by col.
#   We just have to sum up the data col by col and ensure that it provides the value 1.
#
# *round* function is only use to prevent round issue.
if(round(mean(colSums(fi))) == 1) print("Ok the sum of the whole sample set OMEGA equal ONE :)")

##
# Graph of the Theoretical distribution
##
#
#   * [DistributionSymRanWal]: data.frame wich contains two columns:
#     + [Value] All the possible value that the random walk could take at time = k (after k coin tosses)
#     + [Frequency] The associated probability
#
#   * [range]: Number of possibility of value for the random variable at t = k
#
#   * [lastToss]: Only the last coin toss is interesting to graph the distribution
#
range <- dim_x
lastToss <- length(dim_y)

distributionSymRanWal <- data.frame(
  Value = Mt[range, lastToss],
  Frequency = fi[range, lastToss]
)

# For the sake of visibility the limit of X axis has been set to [-100, 100]
file <- paste(figure, 'SymmetricRandomWalkDistribution.pdf', sep = '/')

pdf(file = file)

ggplot(data = distributionSymRanWal, aes(Value, Frequency)) +
  geom_line() +
  scale_x_continuous(limits = c(-100, 100))

dev.off()

############################ 
# Increments               #
############################ 

# Table of increments of the Symmetric Random Walk designed abovoe (Mk)
X
# Xk is the possible outcome of the random variable X:
Xk <- c(1, -1)
# Expectation:
Ex <- weighted.mean(Xk, c(p, q))
Ex.square <- weighted.mean(Xk^2, c(p, q))
# Variance
S <- Ex.square - Ex^2

###########################################################
# Check the martingale property of symmetric random walk. #
###########################################################
#
# The martingale 
# With l > k, show that:
# E[Ml|F(k)] = Mk

##
# first experiment: 
# simple NOT CONDITIONAL Expectation
# E[M200|f(0)] should equal zero because:
# M0 = 0 and Because Mt is a martingale thus:
##
EM200 = sum(Mt[1:200, 200] * fi[1:200, 200]) # equal zero.

##
# Next experiment: Conditional expectation
# It means: Expectation of Mt_l at k
# denoted by: E[Mt_l|f(k)], with k < l
#
# Variables
#   * [from] : Because the random variable could take more than one value at t = k, a starting point for expectation must be fixed
#   * [k] : to fix the filtration point F(k)
#   * [l] : Indice of the random variable on which the expectation will be computed
#   * [len]: length between k and l
#   * [df] : reduced data.frame containing data useful for calculation of expectation
#   * [i] : Row index of the reduce data.frame which take only the values useful for the calculation of expectation
#   * [j] ; Such as j but using as column index.
#   * [fi_min] : Shorter version of the probabilities' array used above for Mk variable.
##
# To make another calculation of other expectation the following variables should be changed:
#   * [from] : as starting column point
#   * [k] : As stating row point
#   * [l] : Ending column point. (the ending x-axis point in automatically given by the procedure.)
##
from  <- 2
k     <- 4
l     <- 19
#
# IMPROVE:
# Furhter release of the document should deal with environment
# It could be possible to use lockBinding to make the following variable acting as constant.
#
len     <- abs(l - k)
i       <- from:(from + len)
j       <- k:l
df      <- Mt[i, j]
fi_min  <- fi[1:(len+1), 1:(len+1)]
#
# Finally compute the expectation E[Mt_l|f(k)]:
# Does the starting value equal the expectation up to l ?
#   * [Mt[from, k]]: (from, k) are the coordinate of the starting point from where the calculation of the expectation start.
#
ifelse(Mt[from, k] == sum(df[, l-k+1] * fi_min[, l-k+1]),
       "martingale!",
       'Not a martingale')
 
























