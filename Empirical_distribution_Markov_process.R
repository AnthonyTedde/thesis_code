# library
library(RandomWalk)
library(ggplot2)
library(dplyr)

# Define a serie of sample of Markov Process
# Ultimately the following Markov process will denote the volatility of 
# a stock price path.
markov <- sbmotionGenerator(n= 100)

# Plot all the sample process (just for fun)
ggplot(bind_rows(markov, .id = 'unique_id'),
       aes(x = time_periods, y = brownian_motion_path, group = unique_id)) +
  geom_line()