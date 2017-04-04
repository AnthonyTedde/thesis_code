#########################################
# Example 1.2.4: Probability measure
########################################
# by domain I define the quantiles:
domain <- seq(from = 0,
              to = 1,
              by = 0.01)
# in example 1.2.4, quantiles are value of the RV because:
# X(w) = w and w belongs to [0, 1]

# punif give distribution (cumulative)
plot(x = domain, 
     y = punif(domain),
     type = 'l')

# dunif give density 
plot(domain, dunif(domain))

# define the probability measure
# mux(B in [.4, .8])
# either:
mux <- .8 - .4
# or
punif(.8) - punif(.4)
# even if the author speaks about random variable in this example.
# The thing to keep in mind is how to construct the UNIFORM PROBABILITY MEASURE
# thus with R, two way:
# first, cumulative distribution 
# second, probability density. integral would be needed to compute the probability measure:
# but it can be easily computed because the uniform distribution is... uniform and therefore the figure integrated
# are rectangle
dt <- 0.8 - 0.4
leftHandBound <- dunif(0.4)
integral <- dt * leftHandBound
