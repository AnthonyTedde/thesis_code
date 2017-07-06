################################################################################
#
#  Create a Normaly distributed RV from an Uniformly one.
#  
# PROBABILITY NORMAL TRANSFORM
#
################################################################################
#
# Path to figure repo
figure <- paste(getwd(),'StochasticCalculusForFinanceVol2', 
                'Chapter1', 
                'Exercice1.2.6',
                'figure', 
                sep = '/')

#
# First: Get the Uniformly distributed RV
# The only condition is that its value must be contained in the closed interval:
# [0, 1]
# 
# the more the number of random values the better it is. (999999)... 
a <- runif(999999)

#
# To transform the previously created uniformly random variable to another
# normaly distribution, we will use the inverse of the cumulative function(N^-1)
# In R:
#   * pnorm(q) give the cumulative value accumulated from 0 to q
#       Q are contained in the closed interval [0,1]
#   * qnorm(p): Give the inverse of the cumulative distribution.
#
# In other words: pnorm(qnorm(p)) will return p.
#
# The following RV b is therefore a STANDARD NORMAL RANDOM VARIABLE constructed
# from a Uniformly Distributed RB
b <- qnorm(a)

# 
# It exists differents ways to show that b is truly a standard normal random V.
# 
# First by graphing its distribution:
file <- paste(figure, 'NormalDistribFromUniformRV.pdf', sep = '/')
pdf(file)
hist(b)
dev.off()