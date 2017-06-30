################################
#
# Scaled Symmetric Random Walk
#
################################
#
#       Wnt = 1/sqrt(n) * Mnt
# With:
#       Mnt = \sum_(j=1)^k=nt Xj
#
# An analogy could therefore be made between symmetric random walk and 
# scaled SRW. 
#
# As previously we start with the random variable X.
# Theoricall X can be constructed the same way it is with symmetric
# Random Walk:
#
#   * [Wnt_s]: Scaled symmetric random walk (sample)
#   * [Wnt_rv]: Scaled symmetric random walk (random variable)
#   * [n]: Scaled term of the scaled symmetric random walk
#   * [t]: Time term.
#   * []: 
#   * []: 

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
figure <- paste(getwd(),'StochasticCalculusForFinanceVol2', 
                'Chapter3', 
                'figure', 
                sep = '/')


##############################################################
# Sample scaled symmetric random walk                        #
##############################################################

n <- 100
t <- 4
#
# All the previous code come from SymmetricRandomWalk.R
# It would be useful to create a more general function.
#
# Construction on the Empirical Random variable X
k <- n * t
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
Mnt_s <- 1 / sqrt(n) * Mk

##
# Check by plot
##
#
#   * [Mnt_s_df]: data.frame containing:

Mnt_s_df <- data.frame('Mnt' = Mnt_s,
                       't' = seq(0, 4, along.with = 0:(n*t)))
file <- paste(figure, 'EmpiricalScaledSymmetricRandomWalkSample.pdf', sep = '/')
pdf(file = file)
ggplot(data = Mnt_s_df, aes(t, Mnt)) +
  geom_line()
dev.off()

























