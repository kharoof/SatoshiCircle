source("functions.R")

#start_pot is quite arbitrary
start_pot <- 100

sims <- 1000

sapply(1:sims ,martingale_simulation,stopping_time=100, fraction=0.01, debug=FALSE)

sim_results <- sapply(1:sims ,martingale_simulation,stopping_time=100, fraction=0.01, debug=FALSE)

sim_results <- sapply(1:sims
                    , martingale_simulation
                    , stopping_time=100
                    , fraction=0.01
                    , debug=FALSE
                    , circle=twoXCircle
                      )

##Likelihood of making profit
sum(sim_results-start_pot > 0) / sims

## Average Profit
mean(sim_results)
quantile(sim_results)
#plot of density function
plot(density(sim_results))


## Things to Try
## Vary the stopping time, vary the fraction, vary the circle
## Try to maximise the expected value and go for games with a skewness towards the high side
