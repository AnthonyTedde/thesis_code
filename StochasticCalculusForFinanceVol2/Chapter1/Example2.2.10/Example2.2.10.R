################################################################################
# 2.2.10
################################################################################
library(ggplot2)
# X is a Standard Normal Random Variable.
# Z and X are independent.
# Z only take two value (1 and -1). Each of these possibility take probability
# 1/2

# dnorm denotes the normal density
# What is the probability that X takes value between -1 and 1? for the STD norm RB
s <- seq(-4, 4, by = 0.01)
w <- dnorm(s)
dist <- as.data.frame(cbind(s, w))
ggplot(dist, aes(s, w)) + geom_line()
# How to get the integral between -1 and 1 ?
# By using the cumulative distribution (pnorm)
pnorm(1) - pnorm(-1)