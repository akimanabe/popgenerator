#age function

age <- function(n,mean,sd,roundage=FALSE){

  tempage <- abs(rnorm(n,mean,sd))

  if(roundage==TRUE){
    return(ceiling(tempage))
  }
  if(roundage==FALSE){
    return(tempage)
  }

}
