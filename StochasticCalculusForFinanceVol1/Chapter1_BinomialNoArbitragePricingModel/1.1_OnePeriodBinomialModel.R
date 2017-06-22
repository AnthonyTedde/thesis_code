#############################
#                           #
# One-Period Binomial Model #
#                           #
#############################
#
#   * [t0] : Beginning of the period
#   * [t1] : End of the periodod
#   * [S0] : Price of the stock at t0 (Deterministic variable)
#   * [S1] : Price of the stock at t1 (Random variable) can take 2 states:
#     + [S1H] : If coin toss results to HEAD
#     + [S1T] : If coin toss results to TAIL
#   * [u] : Up-factor
#   * [d] : Down-factor. Could not be less that 0 because otherwise
#           There is an arbitrage opportunity
#   * [r] : Interest rate. Could not be less that -1. See rule 1.1.2
#           To understand why.
#
# (!) We do not assume that the coin is faire. It means that the probability
# of head must not be the same as the probability of tails.
# These probabilities are denoted by the following variables:
#
#   * [p] : Probability of HEAD
#   * [q] : Probability of TAIL
#

############################
# Variables Initialisation #
############################
t0 <- 0
t1 <- 1
S0 <- 4
u <- 2
d <- 1/2
r <- 1/4
p <- 0.5

##
# Computed value - Do not touch it even if you are a Jedi
#
S1H <- u * S0
S1T <- d * S0
q <- 1 - p

##
# Check the no arbitrage properties based on rule (1.1.2 - page2)
#
if(
  0 < d &
    d < (1 + r) &
    (1 + r) < u
) print("According to declared varible no arbitrage opportunity exhibit.") else 
  print("Arbitrage opportunity mays occur")







































