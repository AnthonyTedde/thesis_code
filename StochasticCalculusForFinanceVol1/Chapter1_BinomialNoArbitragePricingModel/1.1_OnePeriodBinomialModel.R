#############################
#                           #
# One-Period Binomial Model #
#                           #
#############################
#
#   * [t0]: Beginning of the period
#   * [t1]: End of the periodod
#   * [S0]: Price of the stock at t0 (Deterministic variable)
#   * [S1]: Price of the stock at t1 (Random variable) can take 2 states:
#     + [S1H]: If coin toss results to HEAD
#     + [S1T]: If coin toss results to TAIL
#   * [u]: Up-factor
#   * [d]: Down-factor. Could not be less that 0 because otherwise
#          There is an arbitrage opportunity
#   * [r]: Interest rate. Could not be less that -1. See rule 1.1.2
#          To understand why.
#   * [k]: Strike price
#
#   Unknowns of the model (Nevertheless for the sake of example 1.1.1,
#   some value has been set for those unknowns):
#
#   * [X0]: Intial wealth
#   * [delta0]: # of share of stock that one must hold to replicate
#     the position.
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
t0      <- 0
t1      <- 1
S0      <- 4
k       <- 5
u       <- 2
r       <- 1/4
p       <- 0.5
X0      <- 1.20
delta0  <- 0.5

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


############################
# Replication take place   #
############################
#
# Some definitions:
#
#   * European call option: Options which confers on its owners the right
#       but not the obligation to buy one share of stock at time one for 
#       a predetermined price called the strike price.
#
#   * European put option: Options which confers on its owners the right  
#       but not the obligation to sell one share of stockt at time one for
#       a predertermined price called the stroke.
#
#   * European vs. American: The only difference between these kind of options
#       is that the European option can only be exercised at maturity whereas
#       an American one could be exercised at any time before maturity.
#
# The position in option also are important to be mentioned:
#
#   * Long position: Buyer of the option. In this position one purchase the
#       right but not the obligation to exercise it.
#
#   * Short position: Seller of the option. In this position one must sell or
#       buy the underlying according to the fact that the long position
#       choose to exercises its right or not.
#
# arbitrage trading theory is an approach of the option-pricing problem that
# consists on replicate the option by trading in the stock and money market.
#
# An example using the values defined just above.
# Reminder of the value associated to the different variables:
#     * t0      <- 0
#     * t1      <- 1
#     * S0      <- 4
#     * k       <- 5
#     * u       <- 2
#     * r       <- 1/4
#     * p       <- 0.5
#     * X0      <- 1.20
#     * delta0  <- 0.5

#
# Gain in the case the stock price goes to head at T1
#
# Variables
#   * [InitialPortFolioValue]: The value of the portfolio at T0 (Not random)
#   * [LongPosition]: The long position in the option.
#                     (Mean the holder of the right over the underlying)
#     * ['H']: If the coin toss result to head
#     * ['T']: If the coin toss result to tail
#   * [ShortPosition]: The short position in the option.
#                       (Concerne the one which hold a claim)
#     * ['H']: If the coin toss result to head
#     * ['T']: If the coin toss result to tail
#   * [MoneyMarketValue]: The growth of the value invested / borrowed
#                         to the money market
#     * [+]: The value is positive if one invests in MM
#     * [-]: The value is negative if one borrows in MM
#   * [StockHoldingMarketValue]: Value of the stock according to the delta
#                                 (The amount hold by the hedger)
#     * ['H']: If the coin toss result to head
#     * ['T']: If the coin toss result to tail
#   * [PortfolioValue]: The value of the portfolio at T1 except the value of the
#                       stock option hold or owe.
#     * ['H']: If the coin toss result to head
#     * ['T']: If the coin toss result to tail
#
LongPosition <- c('H' = max(S1H - k, 0),
                  'T'= max(S1T - k, 0))
ShortPosition <- c('H' = min(k - S1H, 0),
                   'T' = min(k - S1T, 0))
#
# If a head occurs:
#   -> The short position value is -3. Thus to replicate the position we must do
#       a trade that provide 3 at T1
# If a tail occurs:
#   -> The short position value is 0. Thus to replicate the position we must do
#       a trade that provide 0 at T1
#

############################
# hedge of short position  #
############################

# At time zero the value of the portfolio is:
#  (This is the only value which is not random)
#
InitialPortfolioValue <- X0 - delta0 * S0

# At Time one if the coin toss results to head/tail we get 
# (as holder of short position):
MoneyMarketValue <- InitialPortfolioValue * (1 + r)
StockHoldingMarketValue <- c('H' = delta0 * S1H,
                             'T' = delta0 * S1T)

# For the following code the name 'H' and 'T' has not been set up
# especially to not duplicate the information inside the vector [PortfolioValue]
#
PortfolioValue <- c(MoneyMarketValue + StockHoldingMarketValue['H'],
                    MoneyMarketValue + StockHoldingMarketValue['T'])

#
# If following check is TRUE then the short position has been replicated:
#
if ( identical(PortfolioValue, -1 * ShortPosition) )
  print(paste('Ok the short position has been replicated and the value', 
               'of the option at T0 should be fixed at',
               paste0('$',X0),
               'to avoid arbitrage', sep = ' ')) else
  print('Fuck off, the computation lead to a universe black hole...')
#
# The previous check verify the hedge and return:
# 
# [1] "Ok the short position has been replicated and the value of the option 
#     at T0 should be fixed at $1.2 to avoid arbitrage"
#

############################
# hedge of long position   #
############################
#
# To hedge the long position one have only to do the reverse of the previous
# strategy
#

############################
# Hedging strategy's eq.   #
############################
#
# Here the equations to place a hedge for short and long position are developped
# First the variable used to model the hegde has to be defined.
#
# Some are unknowns
#   * [X0]: Initial wealth
#   * [delta0]: 
#
# Other though possibly random are knowns and are used as independent variables
# to model the hedge:
#   * [S0]: 
#   * [S1]: 
#   * [V0]: 
#   * [V1]: 
#   * [u]: 
#   * [d]: 
#   * [r]: 
#   * [k]: 
#   * [deltaNeutralProbability]: 
#
# The basis of the model is that X0 must equal V0 to conclude that the hedge
# works.

##
# First: Hedge of the short position in the option.
#
# One first starts with initial wealth [X0] and would buy a certain amount
# of share of stock [delta0]. This operation let him with a cash position of:
#
#   [cashPosition[T0]]: X0 - delta0 * S0
#
#   *[X0]: is unknown
#   *[delta0]: is unknown
#   *[S0]: is known
#
# The prevours initial cash position would be invested (if positive) or borrowed
# (if negative) from the money market. This would let him with a [portfolioValue] 
# at T1:
#
#   [portfolioValue[T1]] = (delta0 * S1) + (1+r)(X0 - delta0 * S0) 
#
# The previous formula could be rewrite as:
#
#   [portfolioValue[T1]] = (1+r) * X0 + delta0 * (S1 - (1+r) * S0)
#
pvh <- (1+r) * X0 + delta0 * (S1H - (1+r) * S0)
pvt <- (1+r) * X0 + delta0 * (S1T - (1+r) * S0)

portfolioValue <- matrix(data = c(pvh, pvt),
                         dimnames = list(c(t1),
                                         c('H', 'T')))
















