library(RandomWalk)
library(StockPriceSimulator)
library(ggplot2)
library(cowplot)

#
# Sampled Random Walk, with:
#  * prob of head: .5 -> Symmetric Random Walk
#
rand_walk <- srwalk(time_to_maturity = 4,
                    seed = 1,
                    scale = 1)

ggplot(rand_walk, aes(time_periods, random_walk_path)) +
  geom_line()

#
# Generate multiple Sampled Symmetric Scaled Random Walk
#
rand_walks <- srwalkGenerator(time_to_maturity = 4,
                              seed = 1,
                              scale = 100,
                              n = 100)

ggplot2::ggplot(dplyr::bind_rows(rand_walks, .id = "uid")
                , ggplot2::aes(x = time_periods, 
                               y = random_walk_path,
                               group = uid)) +
  ggplot2::geom_line(alpha = .5) + 
  ggplot2::labs(title = "Random Walk paths from multiple random experiments", 
                caption = "Random Walks",
                x = 'Time periods',
                y = 'Random Walk')

#
# Generate Theorical Scaled Random Walk
#
rand_walk_t <- trwalkGenerator(time_to_maturity = 4,
                               scale = 100)

interval <- (rand_walk_t$Mt[1] - rand_walk_t$Mt[length(rand_walk_t$Mt)]) / 
  (length(rand_walk_t$Mt) - 1)

ggplot2::ggplot(rand_walk_t, ggplot2::aes(Mt)) +
  ggplot2::geom_histogram(ggplot2::aes(weight = Pr / interval), binwidth = interval) +
  ggplot2::scale_x_continuous(limits = c(-7.5, 7.5)) + 
  ggplot2::stat_function(fun = dnorm,
                         color = "blue",
                         args = list(mean = 0, 
                                     sd = sqrt(4)))


#
# Generate not reproducible sbmotion
#
bm1 <- sbmotion(time_to_maturity = 4, 
                scale = 100, 
                reproducible = F)

ggplot(bm1, aes(time_periods,
                         brownian_motion_path)) +
  geom_point()
#
# Generate reproducible sbmotion
#
# 1. Generate Sampled Browniant Motion
bm1 <- sbmotion(time_to_maturity = 4, 
                scale = 1000, 
                reproducible = T)
bm2 <- sbmotion(time_to_maturity = 4, 
                scale = 2000, 
                reproducible = T)
# Update the bm2 object such as it keeps the same generated data but
# fits to the bm1's timeframe.
sub_bm2 <- bm2[seq(1, nrow(bm2), by = 2), ]
attr(sub_bm2, "scale") <- 100
attr(sub_bm2, 'row.names') <- attr(bm1, 'row.names')
# Test the ressemblance of the 2 (same I hope) Brownian motion
identical(bm1, sub_bm2)

# Plot the similarity
ggplot(bm1, aes(time_periods,
                         brownian_motion_path)) +
  geom_line()






## Generate multiple Brownian Motion

Sampled <- sbmotionGenerator(time_to_maturity = 4, scale = 100, n = 6)


ggplot2::ggplot(dplyr::bind_rows(Sampled, .id = "uniqueID"), 
                ggplot2::aes(x = time_periods, y = brownian_motion_path, group = uniqueID)) + 
  ggplot2::geom_point(alpha=0.5, ggplot2::aes(colour = uniqueID)) + 
  ggplot2::theme(legend.position = 'none') +
  ggplot2::labs(title = 'Multiple Brownian Motions path with time to maturity fixed at 4',
                caption = 'Multiple Brownian Motions paths',
                x = 'Time period',
                y = 'Brownian motion')

# Quad variation, joint density, mgf...