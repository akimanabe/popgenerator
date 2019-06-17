age_poison <- function(n,lambda){

  tempage <- rpois(n, lambda) + 1
  return(tempage)

}

age_normal <- function(n,mean,sd,roundage=FALSE){

  tempage <- abs(rnorm(n,mean,sd))
  tempage <- tempage[order(tempage)]

  if(roundage==TRUE){
    return(ceiling(tempage))
  }
  if(roundage==FALSE){
    return(tempage)
  }

}


age_lognormal <- function(n,mean=0,sd=1,roundage=FALSE){

  tempage <-   rlnorm(n,mean,sd)

  if(roundage==TRUE){
    return(ceiling(tempage))
  }
  if(roundage==FALSE){
    return(tempage)
  }
}

age_uniform <- function(n,min,max,roundage=FALSE){

  tempage <-   runif(n,min,max)

  if(roundage==TRUE){
    return(ceiling(tempage))
  }
  if(roundage==FALSE){
    return(tempage)
  }
}
