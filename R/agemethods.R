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


age <- function(n,mean,max,min,lamda,sd,method=c("Normal","Lognormal","Poisson","Uniform"),roundage=FALSE){

  if(is.na(method)==TRUE){method=="Normal"}

  if(method=="Normal"){
    age_norm(n,mean,sd)
  }

  if(method=="Lognormal"){
    age_lnorm(n,mean,sd)
  }

  if(method=="Poisson"){
    age_pois(n,lambda)
  }

  if(method=="Uniform"){
    age_unif(n,min,max)
  }
}
