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
# Functions                #
############################
runArbitrage_babyRun <- function(d, u, r){
  ##
  # This function exhibits an arbitrage opportunity according to the property
  # (1.1.2)
  # 
  #   * [d] : Downfactor
  #   * [u] : Upfactor
  #   * [r] : Money market interest rate
  #
  # TODO: Implement the function to do the job
  #
  print("Arbitrage exists")
}

############################
# Variables Initialisation #
############################
t0 <- 0
t1 <- 1
S0 <- 4
u <- 2
r <- 1/4
p <- 0.5

##
# Computed value - Do not touch it even if you are a Jedi
#
S1H <- u * S0
S1T <- d * S0
q <- 1 - p
if(!exists('d')) d <- 1/u

##
# Check the no arbitrage properties based on rule (1.1.2 - page2)
# To rule out arbitrage, the following inequalities must hold:
#     0 < d < 1+r < u
#
if(
  0 < d &
    d < (1 + r) &
    (1 + r) < u
) print("According to declared varible no arbitrage opportunity exhibit.") else
  runArbitrage_babyRun()

############################
# Arbitrage                #
############################
#
# Exhibits some arbitrage opportunity.
#
# First one (1.1.2) does not hold for the inequality: d < 1+r
# In other words, we get the following inequalities, which led to
# an arbitrage opportunity:
#       
#     0 < 1+r < d < u
#
# With the previous configuration, one can:
#   1.  Borrow some money in the money market at rate [r]
#   2.  Buy shares of stock that provides in the worst case a return of:
#       [d*S0].
#   3.  The return provided by the share at time 1 is much that the money owe to
#       the money market
#
#
# Next, consider the inequality: 0 < d < u < 1+r.
#
# Particularly this inequality means that it would be more profitable
# to invest in money market than in stock. A paradoxical issue arise because 
# investing in mony market security brings less risk than investing in shares
# and therefore money market investment shoud be less valueable.
#
# However if the previous inequality arise (u < 1+r), one achieve an arbitrage
# opportunity with the following strategy:
#   1.  Sell short the stock.
#   2.  Investing the money provided by the short selling of the stock
#       in a money market account.
#   3.  At maturity (t=1) receive the paiment of the principal + the interest
#       and use it to pay off the short selling.
# As in the best case the stock has rose less than the money market account.
# One finish with a profit without the risk of lost.
#
#
# A definition of arbitrage is provided on page 2:
#   Arbitrage:  Trading strategy that begins with no money, has zero probability 
#               of losing money, and has a positive probability of making money.




































