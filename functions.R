## Function which simulates a spin of the five X circle
fiveXCircle <- function(...){
    random <- sample(1:17,1)
    if(random ==1 ) {result <- 5}
    else if(random %in% c(3,6,11,14) ) {result <- 2}
    else if(random %in% c(8,9) ) {result <- 1.85}
    else {result <- 0}
    return(result)
}

#A Quick test on the fiveXCircle function
#A large number of simulations should result in expected value of close to 0.9823529412
#sum(sapply(1:1000,fiveXCircle))/1000


## Function which takes a starting pot and bets
## each time this is ran, the bet is increased by a specified multiple
## stopping_time is stopping time
## start_pot is the starting pot
## fraction is fraction of the original pot to revet to upon a win
#start_pot = 100

martingale_simulation <- function( stopping_time=5 , fraction=0.01, debug=FALSE, return_vector=FALSE, ...) {
    pot <- start_pot
    pot_vector <- 0
    start_wager <- start_pot*fraction
    last_play_win <- TRUE
    for (i in 1:stopping_time) {
        ## Choose the value to bet
        if(last_play_win == TRUE) {
            wager <- min(start_wager, pot)
        }
        else {
            wager <- min(2 * wager, pot)
        }
        winnings <- wager*fiveXCircle()
        if( winnings > 0 ) { ## This could also be greater than wager
            last_play_win <- TRUE
        }
        else {
            last_play_win<-FALSE
        }
        pot <- max(winnings + pot - wager, 0)
        if(debug==TRUE) {
            print (paste("Wager=",wager))
            print (paste("Winnings=",winnings))
            print (paste("Pot=",pot))
        }
       pot_vector <- c(pot_vector, pot)
    }
    if(return_vector==FALSE) {
        return(pot)
    }
    else {
        return(pot_vector)
    }
}

