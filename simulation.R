source("functions.R")

plot(martingale_simulation(stopping_time=50, return_vector=TRUE))
martingale_simulation(stopping_time=50, return_vector=TRUE)


start_pot = 100
hist(sapply(1:1000 ,martingale_simulation,stopping_time=10, fraction=0.02, debug=FALSE)-start_pot)

plot(density(sapply(1:1000 ,martingale_simulation,stopping_time=2, fraction=0.02, debug=FALSE)-start_pot))


## We want to maximise this sum subject to stopping_time and fraction
stopping_time = 1:20
fraction = seq(0.001, 0.5, by =0.001)

sum(sapply(1:100 ,martingale_simulation,stopping_time=2, fraction=0.02, debug=FALSE)-start_pot)/start_pot

## Apply 10k sims of the martingale betting strategy with a stopping time of 100,
## i.e. cashout after 100 games, and what is the average outcome of this betting strategy?
sim_results <- sapply(1:10000 ,martingale_simulation,stopping_time=100, fraction=0.01, debug=FALSE)


plot(density(sim_results))


sim_results <- sapply(1:10000 ,martingale_simulation,stopping_time=20, fraction=0.01, debug=FALSE)
plot(density(sim_results))


sim_results_20 <- sapply(1:10000 ,martingale_simulation,stopping_time=20, fraction=0.01, debug=FALSE)
plot(density(sim_results_20))
sum(sim_results_20-100 > 0)

sim_results_10 <- sapply(1:10000 ,martingale_simulation,stopping_time=10, fraction=0.01, debug=FALSE)
plot(density(sim_results_10))
##Likelihood of making profit
sum(sim_results_10-100 > 0) / 10000
## Average Profit
sum(sim_results_10) / sum(sim_results_10-100 > 0)

sim_results_30 <- sapply(1:10000 ,martingale_simulation,stopping_time=30, fraction=0.01, debug=FALSE)
plot(density(sim_results_30))
sum(sim_results_30-100 > 0)
## Basically the longer you bet the more likey you are to lose.

