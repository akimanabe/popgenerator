age_pois <- function(n,lambda){

  rpois(n, lambda) + 1

}

age_norm <- function(n,mean,sd){

  abs(rnorm(n,mean,sd))

}

age_lnorm <- function(n,mean=0,sd=1){

  rlnorm(n,mean,sd)

}

age_unif <- function(n,min,max){

  runif(n,min,max)

}
